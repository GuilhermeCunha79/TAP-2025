import pj.generators.{SimpleTypeGenerator, TaskScheduleGenerator}

val x = SimpleTypeGenerator.HumanResourceIdGenerator.sample
println(x.map(_.to))


val y = SimpleTypeGenerator.ProductNameGenerator.sample
println(y.map(_.to))

val z = TaskScheduleGenerator.generateDomainData.sample
println(z.map(_._1))
println(z.map(_._2))
println(z.map(_._3))
println(z.map(_._4))
println(z.map(_._5))