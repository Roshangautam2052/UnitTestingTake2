class TaxCalculator {

  // Tax bands (simplified to make testing a bit easier)
  private val personalAllowance: Int = 10000
  private val basicRateLimit: Int = 50000
  private val higherRateLimit: Int = 125000

  // Tax rates
  private val personalAllowanceRate: Double = 0
  private val basicRate: Double = 0.2
  private val higherRate: Double = 0.4
  private val additionalRate: Double = 0.45

  // A method to calculate the total amount of tax to be paid, returned as a double
  def calculateTax(income: Double): Double = {
    if(income>=0) {
      if (income == 0 || income <= personalAllowance) 0
      else if (income <= basicRateLimit ) {
        val basicRateTax: Double = (income - personalAllowance) * basicRate
        basicRateTax
      }
      else if (income <= higherRateLimit) {
        val taxableBasicIncome: Double = (basicRateLimit-personalAllowance)
        val taxableHigherIncome: Double = (income-basicRateLimit)
        val totalTax = taxableHigherIncome * higherRate + taxableBasicIncome * basicRate
        totalTax
      }
      else {
        val taxableBasicIncome: Double = (basicRateLimit - personalAllowance) // 40000
        val taxableHigherIncome: Double = (higherRateLimit - basicRateLimit) //75000
        val additionalTaxableIncome: Double = (income - higherRateLimit)
        val totalTax = taxableBasicIncome * basicRate + taxableHigherIncome * higherRate + additionalTaxableIncome * additionalRate
        totalTax
      }
    }
    else {
      val errorCode: Double = 404.0
      errorCode
    }
  }

  // A method which can tell you if someone is a higher rate taxpayer
  def isHigherRateTaxpayer(income: Double): Boolean = {
    if(income>=0){
      if(income> higherRateLimit) true
      else false
    }
    else {
      false
    }
  }

  // A method that will return a string with the income limit of their current tax band.
  // The return will also be formatted, E.g: "£12,500" or "No limit"
  def formattedCurrentTaxAllowance(income: Double): String = {
    if(income>=0){
      if(income<= 10000) "£10,000"
      else if(income<= 50000) "£50,000"
      else if(income<=125000) "£120,000"
      else{
        "No Limit"
      }
    }
    else{
      "Error in Input!!"
    }
  }

}
