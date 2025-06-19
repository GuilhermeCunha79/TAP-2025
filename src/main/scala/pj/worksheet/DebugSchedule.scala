package pj.worksheet

import pj.domain.*
import pj.domain.schedule.*
import pj.io.FileIO
import java.io.File

object DebugSchedule:
  def main(args: Array[String]): Unit =
    val dirPath = "C:\\Users\\zpedr\\OneDrive\\Ambiente de Trabalho\\tap_proj\\tap-pj-ncf-12\\files\\assessment\\ms03\\validAgenda_65_in.xml"
    val dir = new File(dirPath)

    if dir.exists then
      if dir.isDirectory then
        val files = dir.listFiles().filter(f => f.getName.startsWith("validAgenda") && f.getName.endsWith("in.xml"))

        if files.isEmpty then
          println("Nenhum ficheiro válido encontrado.")
        else
          files.foreach(processFile)
      else if dir.isFile then
        processFile(dir)
    else
      println(s"Caminho inexistente: $dirPath")

  def processFile(file: File): Unit =
    println(s"A processar ficheiro: ${file.getName}")

    FileIO.load(file.getAbsolutePath) match
      case Right(xml) =>
        val outputFileName = file.getName.replace("_in", "_out")
        ScheduleMS03.create(xml, outputFileName) match
          case Right(s) =>
            println(s"Agendamento criado com sucesso para ${file.getName}")
          case Left(error) =>
            println(s"Erro ao criar o agendamento para ${file.getName}: $error")

      case Left(error) =>
        println(s"Erro ao carregar o ficheiro ${file.getName}: $error")
