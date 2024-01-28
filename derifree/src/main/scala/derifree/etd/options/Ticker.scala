package derifree.etd.options

enum Ticker:
  case Stock(name: String) extends Ticker
  case Index(name: String) extends Ticker

  def symbol: String = this match
    case Stock(name) => name
    case Index(name) => name
