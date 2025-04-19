import pj.domain.*
import pj.domain.resources.Types.*
import pj.domain.schedule.*
import pj.io.FileIO
import pj.xml.*

//val filePath = "C:\\Users\\Utilizador\\Documents\\TAP_Project\\files\\assessment\\ms01\\validAgenda_01_in.xml"
//val filePath = "C:\\Users\\migue\\Desktop\\Universidade\\Mestrado GitHub Projects\\2 Ano\\2 Semestre\\TAP\\tap-pj-ncf-12\\files\\assessment\\ms01\\validAgenda_01_in.xml"
// Caminho do arquivo XML
val filePath = "C:\\Users\\Guilherme Cunha\\IdeaProjects\\tap-pj-ncf-12\\files\\assessment\\ms01\\invalidAgenda_02_in.xml"

// Carregar o arquivo XML
val result = FileIO.load(filePath)

// Tratar o resultado: verificar se é um Right( Elem) ou Left(erro)
result match {
  case Right(xml) =>
    // Se o XML foi carregado com sucesso, passar para a função create
    val schedule = ScheduleMS01.create(xml)
    schedule match {
      case Right(s) => println(s"Agendamento criado com sucesso: $s")
      case Left(error) => println(s"Erro ao criar o agendamento: $error")
    }

  case Left(error) =>
    // Caso haja erro ao carregar o XML
    println(s"Erro ao carregar o arquivo XML: $error")
}

