package models

import scala.util.Random

 case class Runner (
                    height: Int,
                    weight: Int,
                    stamina: Int
                   ){
   val titles = Seq("Mr", "Lord", "Lordess", "Mistress", "Lady", "Lad", "Mrs", "Miss", "Mister", "Mad", "Madame", "Sir", "Squire", "Peasant", "Really poor peasant", "Hobo", "Envious", "Horrendously Tired", "Thirsty", "Starving", "Easily Annoyed", "Prone to Disease")
   val firstName = Seq("Steve", "Jerememy", "Bob", "Dan", "Ladon", "Rut", "Wheeldabob", "Meat", "Bun", "Juice", "Pendergrast", "Niel", "Chris", "Sam", "Dave", "Liz", "Scott", "Abdi", "Andrew", "Chi Chi", "Kieran", "ToTo", "Ricardo", "Lisa", "Mona", "Patricia", "Alyss", "Coriander")
   val surname = Seq("Squirtle", "Charmander", "Bulbasaur", "Hunter", "Butcher", "Smelter", "Rod", "Rubio", "Rabbio", "Grabber", "Ponyta", "Zapdos", "Articuno", "Moltres", "Bagel", "Wolf", "Breeze", "Wind", "Ink", "Snow", "Holder", "Bender", "Winner", "Loser")
   val name: String = nameGenerator

   def nameGenerator : String = Random.nextInt(100000).toString +" "+titles(Random.nextInt(titles.length))+" "+firstName(Random.nextInt(firstName.length))+" "+surname(Random.nextInt(surname.length))

   def legLength: Double = (height/30.47)/2

   def calcStep: Double = legLength * (5 * getBMIRatingMultiplier)

   def calcBMI: Double = weight / (H2M*H2M)

   def H2M: Double = height/100

//   object BMIRating {
//     val underweight: Set[Int] = (0 until 19).toSet
//     val average: Set[Int] = (19 until 25).toSet
//     val overweight: Set[Int] = (25 to 30).toSet
//     val unknown = Set()
//
//     val categories = Map(underweight -> Some("Underweight"), average -> Some("Healthy"), overweight -> Some("Overweight"), unknown -> Some("Obese"))
//
//     def unapply(bmi: Int): Option[String] = categories(
//       categories
//         .keys
//         .find(intSet => intSet.contains(bmi))
//         .getOrElse(unknown)
//     )
//   }

//   def getBMIRating: String = {
//     calcBMI match {
//       case BMIRating(rating) => rating
//       case _ => "BMI out of range"
//     }
//   }

  def getBMIRating: String = {
    calcBMI match {
      case rating if rating <=19 => "Underweight"
      case rating if rating> 19 && rating <=25 => "Healthy"
      case rating if rating> 25 && rating <=30 => "Overweight"
      case _ => "Obese"
    }
   }

   def getBMIRatingMultiplier: Double = {
     getBMIRating match{
       case "Underweight" => 1.1
       case "Healthy" => 1.25
       case "Overweight" => 1.1
       case "Obese" => 0.9
     }
   }

   def calcHundredSteps: Double = {
     val fatiguedSteps = 100 - stamina
     val deathsDoor = if (fatiguedSteps > 80) 100 - fatiguedSteps else 0
     val fastSteps = calcStep * stamina
     val slowSteps = (calcStep * (fatiguedSteps - deathsDoor)) * 0.9
     val deathSteps = if (deathsDoor > 0) (calcStep * deathsDoor) * 0.8 else 0

     fastSteps + slowSteps + deathSteps
   }
}

