package derifree.fd

final case class Settings(
    gridQuantile: Double = 1e-5,
    nRannacherSteps: Int = 2
)

object Settings:

  val default: Settings = Settings()
