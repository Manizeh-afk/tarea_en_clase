
object Util {
  def redondear(numero: Double): Double =
    BigDecimal(numero).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
}

trait Figura {
  def area: Double
}

class Rectangulo(val base: Double, val altura: Double) extends Figura {
  def area: Double = base * altura
}

class Circulo(val radio: Double) extends Figura {
  def area: Double = Math.PI * radio * radio
}

class Triangulo(val base: Double, val altura: Double) extends Figura {
  def area: Double = base * altura / 2
}

object Figuras {
  def mostrarArea(figura: Figura): Double = figura.area
}

abstract class Animal(val nombre: String) {
  def sonido(): String
}

trait Volador {
  def volar(): String = "Estoy volando"
}

trait Nadador {
  def nadar(): String = "Estoy nadando"
}

class Perro(nombre: String) extends Animal(nombre) {
  def sonido(): String = "Guau"
}

class Pato(nombre: String) extends Animal(nombre) with Volador with Nadador {
  def sonido(): String = "Cuac"
}

class Pez(nombre: String) extends Animal(nombre) with Nadador {
  def sonido(): String = "Blub"
}

abstract class Empleado(val nombre: String, val salarioBase: Double) {
  def salarioTotal: Double
}

class Desarrollador(nombre: String, salarioBase: Double, val bono: Double)
    extends Empleado(nombre, salarioBase) {
  def salarioTotal: Double = salarioBase + bono
}

class Vendedor(nombre: String, salarioBase: Double, val ventas: Double, val comision: Double)
    extends Empleado(nombre, salarioBase) {
  def salarioTotal: Double = salarioBase + ventas * comision
}

class Gerente(nombre: String, salarioBase: Double, val porcentajeBono: Double)
    extends Empleado(nombre, salarioBase) {
  def salarioTotal: Double = salarioBase + salarioBase * porcentajeBono
}

trait Notificacion {
  def enviar(mensaje: String): String
}

class Email extends Notificacion {
  def enviar(mensaje: String): String = s"Enviando correo: $mensaje"
}

class SMS extends Notificacion {
  def enviar(mensaje: String): String = s"Enviando SMS: $mensaje"
}

class Push extends Notificacion {
  def enviar(mensaje: String): String = s"Enviando notificación: $mensaje"
}

object Notificaciones {
  def procesarNotificacion(notificacion: Notificacion, mensaje: String): String =
    notificacion.enviar(mensaje)
}

abstract class Vehiculo(val marca: String, val velocidadMaxima: Double) {
  def descripcion(): String
}

trait Electrico {
  def capacidadBateriaKWh: Double
  def consumoKWhPor100Km: Double

  def autonomiaKm: Double =
    capacidadBateriaKWh / consumoKWhPor100Km * 100
}

trait Recargable {
  def potenciaCargaKW: Double
  def capacidadBateriaKWh: Double

  def tiempoCargaHoras: Double =
    capacidadBateriaKWh / potenciaCargaKW

  def recargar(): String =
    s"Recargando batería (carga completa en ${Util.redondear(tiempoCargaHoras)} h)"
}

class Automovil(marca: String, velocidadMaxima: Double, val puertas: Int)
    extends Vehiculo(marca, velocidadMaxima) {

  def descripcion(): String =
    s"Automóvil $marca - $velocidadMaxima km/h - $puertas puertas"
}

class Motocicleta(marca: String, velocidadMaxima: Double, val cilindrada: Int)
    extends Vehiculo(marca, velocidadMaxima) {

  def descripcion(): String =
    s"Motocicleta $marca - $velocidadMaxima km/h - $cilindrada cc"
}

class AutoElectrico(
    marca: String,
    velocidadMaxima: Double,
    val capacidadBateriaKWh: Double,
    val consumoKWhPor100Km: Double,
    val potenciaCargaKW: Double
) extends Vehiculo(marca, velocidadMaxima) with Electrico with Recargable {

  def descripcion(): String =
    s"Auto eléctrico $marca - $velocidadMaxima km/h - autonomía ${Util.redondear(autonomiaKm)} km"
}

object Clase20Ejercicios {

  def ejercicio1(): Unit = {
    val figuras = List(
      new Rectangulo(4, 5),
      new Circulo(3),
      new Triangulo(6, 4)
    )

    val areas = figuras.map(figura => Util.redondear(Figuras.mostrarArea(figura)))

    println("E1 áreas (rectángulo | círculo | triángulo): " + areas.mkString(" | "))
  }

  def ejercicio2(): Unit = {
    val perro = new Perro("Rocky")
    val pato = new Pato("Donald")
    val pez = new Pez("Nemo")

    val animales = List(
      s"${perro.nombre}: ${perro.sonido()}",
      s"${pato.nombre}: ${pato.sonido()}, ${pato.volar()}, ${pato.nadar()}",
      s"${pez.nombre}: ${pez.sonido()}, ${pez.nadar()}"
    )

    println("E2: " + animales.mkString(" | "))
  }

  def ejercicio3(): Unit = {
    val empleados: List[Empleado] = List(
      new Desarrollador("Ana", 5000, 800),
      new Vendedor("Luis", 3000, 20000, 0.05),
      new Gerente("Marta", 8000, 0.15)
    )

    val detalle = empleados.map(empleado =>
      s"${empleado.nombre} = ${empleado.salarioTotal}"
    ).mkString(" | ")

    val total = empleados.map(_.salarioTotal).sum

    println(s"E3: $detalle | Total = $total")
  }

  def ejercicio4(): Unit = {
    val servicios: List[Notificacion] = List(
      new Email,
      new SMS,
      new Push
    )

    val mensajes = servicios.map(servicio =>
      Notificaciones.procesarNotificacion(servicio, "Hola")
    )

    println("E4: " + mensajes.mkString(" | "))
  }

  def ejercicio5(): Unit = {
    val vehiculos: List[Vehiculo] = List(
      new Automovil("Toyota", 180, 4),
      new Motocicleta("Yamaha", 160, 250),
      new AutoElectrico("Tesla", 250, 75, 15, 11)
    )

    val tesla = new AutoElectrico("Tesla", 250, 75, 15, 11)

    println(
      "E5: " + vehiculos.map(_.descripcion()).mkString(" | ") +
        " | " + tesla.recargar()
    )
  }

  def main(args: Array[String]): Unit = {
    ejercicio1()
    ejercicio2()
    ejercicio3()
    ejercicio4()
    ejercicio5()
  }
}
