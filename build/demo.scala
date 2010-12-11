import org.tearne.beaner.plant._
import org.tearne.beaner.cross._
import org.tearne.beaner.chroma._

//
// Setup
//
PlantPair.setPlantCrosser(new PlantCrosser(new ChromosomeCrosser()))
ParentPlant.setPlantType(PlantSpec.phaseolusVulgaris)

// Plants
val parent1 = new ParentPlant()
val parent2 = new ParentPlant()
val prefVar = new ParentPlant()

// Criteria
val aphid = new Criterion(parent1, 0, 9)
val blight= new Criterion(parent2, 1, 39)
val criteria = aphid + blight

//
// Do crossings
//
//Heterozygous selection
var f1  = parent1 x parent2 selectHet criteria
var bc1 = f1 x prefVar selectHet criteria
var bc2 = bc1 x prefVar selectHet criteria

//Homozygous selection
var fin = bc2 x bc2 selectHom criteria

//
// Results
//
println(fin.proportionOf(prefVar))
