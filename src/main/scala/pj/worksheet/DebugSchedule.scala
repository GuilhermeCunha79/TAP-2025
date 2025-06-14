package pj.worksheet

import pj.domain.*
import pj.domain.schedule.*
import pj.io.FileIO

object DebugSchedule:
  def main(args: Array[String]): Unit =
    // val filePath = "C:\\Users\\Utilizador\\Documents\\TAP_Project\\files\\assessment\\ms01\\validAgenda_01_in.xml"
    //val filePath = "C:\\Users\\migue\\Desktop\\Universidade\\Mestrado GitHub Projects\\2 Ano\\2 Semestre\\TAP\\tap-pj-ncf-12\\files\\assessment\\ms03\\invalidPhysicalResourceUnavailable_in.xml"
    // val filePath = "C:\\Users\\Guilherme Cunha\\IdeaProjects\\tap-pj-ncf-12\\files\\assessment\\ms01\\invalidAgenda_02_in.xml"
    val filePath = "C:\\Users\\zpedr\\OneDrive\\Ambiente de Trabalho\\tap_proj\\tap-pj-ncf-12\\files\\assessment\\ms03\\invalidPhysicalResourceUnavailable_in.xml"
    val result = FileIO.load(filePath)

    result match
      case Right(xml) =>
        val schedule = ScheduleMS03.create(xml)
        schedule match
          case Right(s) =>
            println(s"Agendamento criado com sucesso: $s")
          case Left(error) =>
            println(s"Erro ao criar o agendamento: $error")

      case Left(error) =>
        println(s"Erro ao carregar o arquivo XML: $error")
