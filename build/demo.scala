import org.tearne.beaner.plant._
import org.tearne.beaner.cross._
import org.tearne.beaner.chroma._
import org.tearne.beaner.report._

PlantPair.setPlantCrosser(new PlantCrosser(new ChromosomeCrosser()))

// --- Setup ---
// Starting plants
val pV = PhaseolusVulgaris("Pref Var")
val p1 = PhaseolusVulgaris("Donor1")
val p2 = PhaseolusVulgaris("Donor2")
val p3 = PhaseolusVulgaris("Donor3")
val p4 = PhaseolusVulgaris("Donor4")
// Selection criteria
val c1 = new Criterion(p1, 1, 9)
val c2 = new Criterion(p2, 3, 50)
val c3 = new Criterion(p3, 4, 24)
val c4 = new Criterion(p4, 7, 36)
val cAll = c1 + c2 + c3 + c4


// --- Crossings ---
// F1s
val f1_p1p2 = (p1 x p2) selectHet (c1 + c2) named "F1_p1p2"
val f1_p3p4 = (p3 x p4) selectHet (c3 + c4) named "F1_p3p4"
val f1_p1p2p3p4 = (f1_p1p2 x f1_p3p4) selectHet cAll named "F1_p1p2p3p4"
// Backcrossing
val bc1 = f1_p1p2p3p4 x pV selectHet cAll named "BC1"
val bc2 = bc1 x pV selectHet cAll named "BC2"
val bc3 = bc2 x pV selectHet cAll named "BC3"
val bc4 = bc3 x pV selectHet cAll named "BC4"
// Selfing
val fin = bc4 x bc4 selectHom cAll named "Final"

// --- Reporting ---
// Prepare for reporting
val colours = new Colour(cAll, pV)
val plantsList = List(
  pV, p1, p2, p3, p4,
  f1_p1p2, f1_p3p4, f1_p1p2p3p4,
  bc1, bc2, bc3, bc4,
  fin
)
// Produce PDF
new Reporter(plantsList, cAll, colours).makePDF()

Thread.sleep(10000)
