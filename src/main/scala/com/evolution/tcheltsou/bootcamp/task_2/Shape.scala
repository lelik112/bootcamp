package com.evolution.tcheltsou.bootcamp.task_2

object Shape {

  sealed trait Shape extends Located with Bounded {
    def area: Double
  }

  sealed trait Located {
    self: Bounded =>
    def x = minX: Double
    def y = minY: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable [A <: Located] {
    self: A =>
    def move(dx: Double, dy: Double): A
  }

  sealed trait Shape3D extends Located3D with Bounded3D {
    def volume: Double
    def surfaceArea: Double
  }

  sealed trait Located3D extends Located {
    self: Bounded3D =>
    def z = minZ: Double
  }

  sealed trait Bounded3D extends Bounded {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable3D [A <: Located3D] {
    self: A =>
    def move(dx: Double, dy: Double, dz: Double): A
  }

  abstract class AbstractPoint(x: Double, y: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def area: Double = 0.0
  }

  final case class Point(override val x: Double, override val y: Double) extends AbstractPoint(x, y)
    with Movable[Point] {

    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)
  }

  abstract class AbstractPoint3D(x: Double, y: Double, z: Double) extends AbstractPoint(x, y) with Shape3D {
    override def maxZ: Double = z
    override def minZ: Double = z

    override def volume:      Double = 0.0
    override def surfaceArea: Double = 0.0
  }

  final case class Point3D(override val x: Double, override val y: Double, override val z: Double)
    extends AbstractPoint3D(x, y, z) with Movable3D[Point3D] {

    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
  }

  case object Origin extends AbstractPoint3D(0.0, 0.0, 0.0)

  abstract class AbstractCircle(center: AbstractPoint, radius: Double) extends Shape {
    require(radius >= 0)

    override def minX: Double = center.x - radius
    override def maxX: Double = center.x + radius
    override def minY: Double = center.y - radius
    override def maxY: Double = center.y + radius

    override def area: Double = Math.PI * radius * radius
  }

  case class Circle(center: Point, radius: Double) extends AbstractCircle(center, radius) with Movable[Circle] {
    override def move(dx: Double, dy: Double): Circle =
      Circle(center.move(dx, dy), radius)
  }

  abstract class AbstractSphere(center: AbstractPoint3D, radius: Double) extends AbstractCircle(center, radius)
    with Shape3D {
    require(radius >= 0)

    override def minZ: Double = center.z - radius
    override def maxZ: Double = center.z + radius

    override def surfaceArea: Double = Math.PI * radius * radius * 4
    override def volume:      Double = Math.PI * radius * radius * radius * 4 / 3
  }

  final case class Sphere(center: Point3D, radius: Double) extends AbstractSphere(center, radius)
    with Movable3D[Sphere] {

    override def move(dx: Double, dy: Double, dz: Double): Sphere =
      Sphere(center.move(dx, dy, dz), radius)
  }

  abstract class AbstractRectangle(a: AbstractPoint, b: AbstractPoint) extends Shape {
    override def minX: Double = a.x min b.x
    override def maxX: Double = a.x max b.x
    override def minY: Double = a.y min b.y
    override def maxY: Double = a.y max b.y

    def length: Double = maxX - minX
    def width:  Double = maxY - minY

    override def area: Double = length * width
  }

  final case class Rectangle(a: Point, b: Point) extends AbstractRectangle(a, b) with Movable[Rectangle] {
    override def move(dx: Double, dy: Double): Rectangle =
      Rectangle(a.move(dx, dy), b.move(dx, dy))
  }

  final case class Square(a: Point, side: Double) extends AbstractRectangle(a, a.move(side, side)) with Movable[Square] {
    require(side >= 0)

    override def move(dx: Double, dy: Double): Square =
      Square(a.move(dx, dy), side)
  }

  abstract class AbstractCuboid(a: AbstractPoint3D, b: AbstractPoint3D) extends AbstractRectangle(a, b) with Shape3D {
    override def minZ: Double = a.z min b.z
    override def maxZ: Double = a.z max b.z

    def height: Double = maxZ - minZ

    override def surfaceArea: Double = (area + (length * height) + (width * height)) * 2
    override def volume:      Double = area * height
  }

  final case class Cuboid(a: Point3D, b: Point3D) extends AbstractCuboid(a, b) with Movable3D[Cuboid] {
    override def move(dx: Double, dy: Double, dz: Double): Cuboid =
      Cuboid(a.move(dx, dy, dz), b.move(dx, dy, dz))
  }

  final case class Cube(a: Point3D, side: Double) extends AbstractCuboid(a, a.move(side, side, side))
    with Movable3D[Cube] {
    require(side >= 0)

    override def move(dx: Double, dy: Double, dz: Double): Cube =
      Cube(a.move(dx, dy, dz), side)
  }

  abstract class AbstractTriangle(a: AbstractPoint, b: AbstractPoint, c: AbstractPoint) extends Shape {
    override def minX: Double = a.x min b.x min c.x
    override def maxX: Double = a.x max b.x max c.x
    override def minY: Double = a.y min b.y min c.y
    override def maxY: Double = a.y max b.y max c.y

    override def area: Double = ???
  }

  final case class Triangle(a: Point, b: Point, c: Point) extends AbstractTriangle(a, b, c) with Movable[Triangle] {
    override def move(dx: Double, dy: Double): Triangle =
      Triangle(a.move(dx, dy), b.move(dx, dy), c.move(dx, dy))
  }

  abstract class AbstractTriangle3D(a: AbstractPoint3D, b: AbstractPoint3D, c: AbstractPoint3D)
    extends AbstractTriangle(a, b, c) with Shape3D {

    override def minZ: Double = a.z min b.z min c.z
    override def maxZ: Double = a.z max b.z max c.z

    override def volume:      Double = 0.0
    override def surfaceArea: Double = ???
  }

  final case class Triangle3D(a: Point3D, b: Point3D, c: Point3D) extends AbstractTriangle3D(a, b, c)
    with Movable3D[Triangle3D] {

    override def move(dx: Double, dy: Double, dz: Double): Triangle3D =
      Triangle3D(a.move(dx, dy, dz), b.move(dx, dy, dz), c.move(dx, dy, dz))
  }

}