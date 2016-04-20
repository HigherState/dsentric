package dsentric

trait LensCompositor[Data] extends Any {

  def f:Data => Data

  def ~(f2:Data => Data):Data => Data =
    (j: Data) => f2(f(j))

  def |>(d:Data) = f(d)
}
