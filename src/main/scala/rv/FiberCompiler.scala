package rv

import scala.language.postfixOps
import scala.util.matching.Regex
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

sealed trait FiberToken

case class THREAD_IDENTIFIER(string: String) extends FiberToken

case class THREAD_STATUS(status: String) extends FiberToken

case class FUNCTION_CALL(string: String) extends FiberToken

case class FUNCTION_CALL_CONTEXT(string: String) extends FiberToken

case object AT extends FiberToken

case object CALL_STACK_DELIMITER extends FiberToken

case object CALL_STACK_LAST extends FiberToken

trait FiberCompilationError

case class FiberLexerError(string: String) extends FiberCompilationError

case class FiberParserError(string: String) extends FiberCompilationError

case class FiberCompilerError(string: String) extends FiberCompilationError

sealed trait FiberAST

case class ThreadIdentifier(string: String) extends FiberAST

case class ThreadStatus(string: String) extends FiberAST

case class FunctionCall(string: String) extends FiberAST

case class FunctionCallContext(string: String) extends FiberAST

case class Function(call: FunctionCall, context: FunctionCallContext) extends FiberAST

case class Thread(id: ThreadIdentifier, status: ThreadStatus, stack: List[Function]) extends FiberAST

case class Threads(lst: List[FiberAST]) extends FiberAST

sealed trait FiberEvent

case class FiberThreadCall(id: String, status: String, function: String, context: String) extends FiberEvent

object FiberLexer extends RegexParsers:
    override def skipWhitespace: Boolean = true

    override val whiteSpace: Regex = "[ \t\r\f]+".r

    private def threadIdentifier: FiberLexer.Parser[THREAD_IDENTIFIER] =
        "\\S*@\\S+".r ^^ { str => THREAD_IDENTIFIER(str) }

    private def threadStatus: FiberLexer.Parser[THREAD_STATUS] =
        "WAITING|RUNNING|YIELDING".r ^^ { str => THREAD_STATUS(str) }

    private def functionCall: FiberLexer.Parser[FUNCTION_CALL] =
        "\\s*[a-zA-Z0-9$]+(?=\\s)".r ^^ { str => FUNCTION_CALL(str) }

    private def functionCallContext: FiberLexer.Parser[FUNCTION_CALL_CONTEXT] =
        "\\S+".r ^^ { str => FUNCTION_CALL_CONTEXT(str) }

    private def at: FiberLexer.Parser[AT.type] =
        "[@]".r ^^ { _ => AT }

    private def callStackDelimiter: FiberLexer.Parser[CALL_STACK_DELIMITER.type] =
        "├".r ^^ { _ => CALL_STACK_DELIMITER }

    private def callStackLast: FiberLexer.Parser[CALL_STACK_LAST.type] =
        "╰".r ^^ { _ => CALL_STACK_LAST }

    private def tokens: FiberLexer.Parser[List[FiberToken]] =
        phrase(rep1(threadStatus | threadIdentifier | functionCall | at | callStackDelimiter | callStackLast | functionCallContext))

    inline def apply(string: String): Either[FiberLexerError, List[FiberToken]] =
        parse(tokens, string) match {
            case NoSuccess(msg, _) => Left(FiberLexerError(msg))
            case Failure(msg, _) => Left(FiberLexerError(msg))
            case Error(msg, _) => Left(FiberLexerError(msg))
            case Success(result, _) => Right(result)
        }

class FiberTokenReader(tokens: Seq[FiberToken]) extends Reader[FiberToken]:
    override def first: FiberToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = NoPosition

    override def rest: Reader[FiberToken] = new FiberTokenReader(tokens.tail)

object FiberParser extends Parsers:
    override type Elem = FiberToken

    private def threadIdentifier: Parser[ThreadIdentifier] =
        accept("thread_identifier", { case id@THREAD_IDENTIFIER(s) => ThreadIdentifier(s) })

    private def threadStatus: Parser[ThreadStatus] =
        accept("thread_status", { case status@THREAD_STATUS(s) => ThreadStatus(s) })

    private def functionCall: Parser[FunctionCall] =
        accept("function_call", { case call@FUNCTION_CALL(s) => FunctionCall(s) })

    private def functionCallContext: Parser[FunctionCallContext] =
        accept("function_call_context", { case ctx@FUNCTION_CALL_CONTEXT(s) => FunctionCallContext(s) })

    private def thread: Parser[FiberAST] =
        threadIdentifier ~ threadStatus ~
            rep1(CALL_STACK_DELIMITER ~ functionCall ~ AT ~ functionCallContext) ~
            rep1(CALL_STACK_LAST ~ functionCall ~ AT ~ functionCallContext) ^^ {
            case id ~ status ~ stack ~ end =>
                val folded = (stack ++ end).foldLeft(Nil)((acc, ele) => {
                    val _ ~ call ~ _ ~ ctx = ele
                    Function(call, ctx) :: acc
                })

                Thread(id, status, folded)
        }

    private def block: Parser[FiberAST] = rep1(thread) ^^ (lst => Threads(lst))

    private def trace: Parser[FiberAST] = phrase(block)

    inline def apply(tokens: Seq[FiberToken]): Either[FiberCompilationError, FiberAST] =
        val reader = new FiberTokenReader(tokens)

        trace(reader) match
            case NoSuccess(msg, _) => Left(FiberParserError(msg))
            case Failure(msg, _) => Left(FiberParserError(msg))
            case Error(msg, _) => Left(FiberParserError(msg))
            case Success(result, _) => Right(result)


object FiberCompiler:
    inline def apply(trace: String): Either[FiberCompilationError, List[FiberEvent]] =
        val ast = for {
            tokens <- FiberLexer(trace.split("\\n").foldLeft("")((acc, ele) => acc + " " + ele))
            ast <- FiberParser(tokens)
        } yield ast
        ast.flatMap {
            case Threads(lst) =>
                val lst1: List[Thread] = lst.foldLeft(Nil)((acc, ele) => ele match
                    case Thread(id, status, stack) => Thread(id, status, stack) :: acc
                    case _ => acc)
                Right(lst1.flatMap {
                    case Thread(id, status, stack) =>
                        val calls: List[FiberThreadCall] = stack.foldLeft(Nil)(
                            (acc, ele) => FiberThreadCall(id.string, status.string, ele.call.string, ele.context.string) :: acc)
                        calls.reverse
                })
            case _ => Left(FiberCompilerError("Expected Threads"))
        }
