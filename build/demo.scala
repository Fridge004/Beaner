import org.tearne.beaner.plant._
import org.tearne.beaner.cross._
import org.tearne.beaner.chroma._
import org.tearne.beaner.report._

//
// Setup
//
PlantPair.setPlantCrosser(new PlantCrosser(new ChromosomeCrosser()))

// Plants
val parent1 = PhaseolusVulgaris()
val parent2 = PhaseolusVulgaris()
val prefVar = PhaseolusVulgaris()

// Criteria
val marker1 = new Criterion(parent1, 0, 9)
val marker2 = new Criterion(parent2, 1, 39)
val criteria = marker1 + marker2

//
// Do crossings
//
//Heterozygous selection
var f1 = parent1 x parent2 selectHet criteria
var bc1 = f1 x prefVar selectHet criteria
var bc2 = bc1 x prefVar selectHet criteria
var bc3 = bc2 x prefVar selectHet criteria
var bc4 = bc3 x prefVar selectHet criteria
var bc5 = bc4 x prefVar selectHet criteria
var bc6 = bc5 x prefVar selectHet criteria

//Homozygous selection
var fin = bc6 x bc6 selectHom criteria

val colours = new Colour(criteria, prefVar)

val plantsList = List(
  Name(prefVar, "Preferred Variety"),
  Name(parent1, "First Donor"),
  Name(parent2, "Second Donor"),
  Name(f1, "F1"),
  Name(bc1, "Backcross 1"),
  Name(bc2, "Backcross 2"),
  Name(bc3, "Backcross 3"),
  Name(bc4, "Backcross 4"),
  Name(bc5, "Backcross 5"),
  Name(bc6, "Backcross 6"),
  Name(fin, "Selfed")
)

new PlantPrinter(plantsList, colours).makePdf()
