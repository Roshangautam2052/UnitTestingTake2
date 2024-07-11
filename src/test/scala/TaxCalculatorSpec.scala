import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TaxCalculatorSpec extends AnyWordSpec {

  val taxCalculator: TaxCalculator = new TaxCalculator

  /**
   * Test cases for calculateTax method
   */
  // I've done the first test for you!
  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when {
      "the income is below the personal Allowance" in {
        val result: Double = taxCalculator.calculateTax(10000)
        assert(result == 0)
      }
    }
  }
  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when{
      "the income is greater than personalAllowance" +
        " and less than or equals to basicRateLimit " in {
        val result : Double = taxCalculator.calculateTax(49000)
        assert(result == 7800)
      }
    }
  }
  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when{
      "the income is greater than basicRateLimit" +
        " and less than or equals to higherRateLimit " in {
        val result : Double = taxCalculator.calculateTax(70000)
        assert(result == 16000)
      }
    }
  }
  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when{
      "the income is greater than higherRateLimit " in {
        val result : Double = taxCalculator.calculateTax(150000)
        assert(result == 49250)
      }
    }
  }
  "TaxCalculator.calculateTax" should {
    "return 404 error " when{
      "the income is less than zero" in {
        val result6 : Double = taxCalculator.calculateTax(-3)
        assert(result6 == 404.0)
      }
    }
  }
  /*
   Test cases for isHigherRateTaxPayer
   */
  "TaxCalculator.isHigherRateTaxPayer" should {
    "return true " when {
      "the income is greater than 125000" in{
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(150000)
        assert(result)
      }
    }
  }
  "TaxCalculator.isHigherRateTaxPayer" should {
    "return false " when {
      "the income is  less than 125000" in{
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(10000)
        assert(!result)
      }
    }
  }
  "TaxCalculator.isHigherRateTaxPayer" should {
    "return false " when {
      "the income is  less than 0" in{
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(-3)
        assert(!result)
      }
    }
  }
  /*
   Test cases for formattedCurrentTaxAllowance
   */
  "TaxCalculator.formattedCurrentTaxAllowance"  should {
    "return income limit of their current tax band " when{
      "income is less than or equal to 10000" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(10000)
        assert(result =="£10,000" )
      }
    }
  }
  "TaxCalculator.formattedCurrentTaxAllowance"  should {
    "return income limit of their current tax band " when{
      "income is greater than 10000 and less than or equal to 50000" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(40000)
        assert(result == "£50,000" )
      }
    }
  }
  "TaxCalculator.formattedCurrentTaxAllowance"  should {
    "return income limit of their current tax band " when{
      "income is greater than 50,000  and less than or equal to 125,000" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(70000)
        assert(result == "£120,000" )
      }
    }
  }
  "TaxCalculator.formattedCurrentTaxAllowance"  should {
    "return income limit of their current tax band " when{
      "income is greater than 125,000 " in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(140000)
        assert(result =="No Limit" )
      }
    }
  }
  "TaxCalculator.formattedCurrentTaxAllowance"  should {
    "return income limit of their current tax band " when{
      "income is less than 0" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(-4)
        assert(result =="Error in Input!!" )
      }
    }
  }

}
