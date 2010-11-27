import org.tearne.beaner.plant._
import org.tearne.beaner.plant.spec._
import org.tearne.beaner.plant.selection._
import org.tearne.beaner.chroma._

val chromaCrosser = new ChromasomeCrosser()

val parent1 = new ParentPlant(PlantSpec.phaseolusVulgaris)
val parent2 = new ParentPlant(PlantSpec.phaseolusVulgaris)
val prefVar = new ParentPlant(PlantSpec.phaseolusVulgaris)

val criteria = List(new Criteria(parent1, 0, 9), new Criteria(parent2, 1, 39))

//Heterozygous selection
var f1  = new PlantCrosser(parent1, parent2, chromaCrosser).selectHeterozygousOffspring(criteria).get
var bc1 = new PlantCrosser(f1,      prefVar, chromaCrosser).selectHeterozygousOffspring(criteria).get
var bc2 = new PlantCrosser(bc1,     prefVar, chromaCrosser).selectHeterozygousOffspring(criteria).get

//Homozygous selection
var fin = new PlantCrosser(bc2,     bc2,     chromaCrosser).selectHomozygousOffspring(criteria).get

println(fin.proportionOf(prefVar))
