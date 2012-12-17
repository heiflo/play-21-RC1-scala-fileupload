package controllers

import play.api._
import libs.Files.TemporaryFile
import libs.iteratee.{Cont, Done, Input, Iteratee}
import mvc.MultipartFormData.FilePart
import play.api.mvc._
import java.io.FileOutputStream

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def upload = Action(multipartPartialFileParser) {
    request =>
      request.body.file("file").map { file =>
        Ok("File uploaded, file size = " + file.ref.file.length + ", file name = " + file.ref.file.getAbsolutePath)
      }.getOrElse {
        Redirect(routes.Application.index).flashing(
          "error" -> "Missing file"
        )
      }
  }

  def multipartPartialFileParser: BodyParser[MultipartFormData[TemporaryFile]] = {
    parse.multipartFormData(handleFilePartAsTemporaryFile)
  }

  def handleFilePartAsTemporaryFile: parse.Multipart.PartHandler[FilePart[TemporaryFile]] = {
    parse.Multipart.handleFilePart {
      case parse.Multipart.FileInfo(partName, filename, contentType) =>
        val tempFile = TemporaryFile("multipartBody", "asTemporaryFile")
        def fold[E, A](state: A)(f: (A, E) => A): Iteratee[E, A] = {
          def step(s: A, count: Int)(i: Input[E]): Iteratee[E, A] = i match {
            case Input.EOF => Done(s, Input.EOF)
            case Input.Empty => Cont[E, A](i => step(s, count)(i))
            case Input.El(e) => {
              val s1 = f(s, e)
              var counter = 0
              e match {
                case  e: Array[Byte] => counter = count + e.length
              }
              if (counter < 300000) {
                Cont[E, A](i => step(s1, counter)(i))
              } else {
                Done(s, Input.EOF)
              }
            }
          }
          (Cont[E, A](i => step(state, 0)(i)))
        }
        fold[Array[Byte], PartialFile](PartialFile(new java.io.FileOutputStream(tempFile.file), 0)) { (partialFile, data) =>
          partialFile.fos.write(data)
          partialFile
        }.mapDone { partialFile =>
          partialFile.fos.close
          tempFile
        }
    }
  }

  case class PartialFile(fos: FileOutputStream, count: Int)
  
}