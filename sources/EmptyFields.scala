package user.sjrd.emptyfields

import com.funlabyrinthe.core.*
import com.funlabyrinthe.core.graphics.*
import com.funlabyrinthe.core.input.*
import com.funlabyrinthe.mazes.*

case object JumpFarBelow extends Ability

val TrapDoorKey = "T"

object EmptyFields extends Module:
  override protected def createComponents()(using Universe): Unit =
    val emptyField = new EmptyField
    val parachutes = new Parachutes
    val parachute = ItemTool.make(
      parachutes, 
      "Avec ce parachute, tu n''hésiteras plus à sauter de haut.",
    )
    val trapDoorField = new TrapDoorField
    val trapDoorsPlugin = new TrapDoorsPlugin
    val trapDoors = new TrapDoors
    val trapDoorTool = ItemTool.make(
      trapDoors,
      s"Tu pourras placer cette trappe sur un trou, ou la reprendre, en appuyant sur $TrapDoorKey.",
    )
  end createComponents
  
  def emptyField(using Universe): EmptyField =
    myComponentByID("emptyField")
  def parachutes(using Universe): Parachutes =
    myComponentByID("parachutes")
  def parachute(using Universe): ItemTool =
    myComponentByID("parachute")
  def trapDoorField(using Universe): TrapDoorField =
    myComponentByID("trapDoorField")
  def trapDoorsPlugin(using Universe): TrapDoorsPlugin =
    myComponentByID("trapDoorsPlugin")
  def trapDoors(using Universe): TrapDoors =
    myComponentByID("trapDoors")
  def trapDoorTool(using Universe): ItemTool =
    myComponentByID("trapDoorTool")
end EmptyFields

export EmptyFields.*

class EmptyField(using ComponentInit) extends Field derives Reflector:
  var usePainter: Boolean = false
  var checkIsMoveAllowed: Boolean = true

  @transient @noinspect
  private val lightenPainter: Painter =
    universe.EmptyPainter + "Filters/NiceSoftLighten"

  override def reflect() = autoReflect[EmptyField]

  protected def findBelow(map: Map, pos: Position): Position =
    if pos.z >= 0 && map(pos).field.isInstanceOf[EmptyField] then
      findBelow(map, pos - (0, 0, 1))
    else
      pos
  end findBelow

  override protected def doDraw(context: DrawSquareContext): Unit =
    import context.*
    
    if usePainter then
      super.doDraw(context)
      DissipateNeighbors.dissipateGroundNeighbors(context)
    else if isSomewhere then
      val map = context.map.get
      val pos = context.pos.get

      val below = findBelow(map, pos)

      if below.z < 0 then
        gc.fill = Color.Black
        gc.fillRect(minX, minY, width, height)
        DissipateNeighbors.dissipateGroundNeighbors(context)
      else
        val belowContext = context.withWhere(Some(map.ref(below)))
        map(below).drawTo(belowContext)

        val depth = Math.min(pos.z - below.z, 5)
        for _ <- 0 until depth do
          lightenPainter.drawTo(context)
      end if
    end if
  end doDraw

  override def entering(context: MoveContext): Unit = {
    import context.*

    val map = context.pos.map
    val pos = context.pos.pos

    val below = findBelow(map, pos)
    val depth = pos.z - below.z

    if below.z < 0 then
      cancel()
      player.showMessage(
        "Tu ne voudrais quand même pas sauter là-dedans !? "
          + "On n''en voit même pas le fond !"
      )
    else if depth > 1 && player.cannot(JumpFarBelow) then
      cancel()
      player.showMessage(
        "Tu ne voudrais quand même pas sauter si bas !? "
          + s"Il y a $depth étages, là !"
      )
    else if checkIsMoveAllowed then
      val belowContext = MoveContext(player, Some(map.ref(below)), keyEvent)
      if !player.testMoveAllowed(belowContext) then
        cancel()
      else
        () // ok
    else
      () // ok
  }

  override def entered(context: MoveContext): Unit = {
    import context.*

    val map = context.pos.map
    val pos = context.pos.pos

    if src.exists(_().field.isInstanceOf[EmptyField]) then
      temporize()

    player.moveTo(map.ref(pos - (0, 0, 1)), execute = true)
  }
end EmptyField

class Parachutes(using ComponentInit) extends ItemDef:
  icon += "Objects/Parachute"
  
  override def perform(player: CorePlayer) = {
    case JumpFarBelow if player.has(this) => ()
  }
end Parachutes

class TrapDoorField(using ComponentInit) extends Field:
  painter += "Fields/WoodFloor"
end TrapDoorField

class TrapDoors(using ComponentInit) extends ItemDef:
  icon += "Objects/TrapDoor"

  override protected def countChanged(player: CorePlayer, previousCount: Int, newCount: Int): Unit =
    player.plugins += trapDoorsPlugin
  end countChanged
end TrapDoors

class TrapDoorsPlugin(using ComponentInit) extends PlayerPlugin:
  override def onKeyEvent(corePlayer: CorePlayer, event: KeyEvent): Unit = {
    val player = corePlayer.reified[Player]
    if player.position.isEmpty then
      ()
    else if event.keyString.toUpperCase() == TrapDoorKey && !event.hasAnyControlKey then
      val map = player.position.get.map
      val playerPos = player.position.get.pos
      
      val optDir = Direction.values.find { dir =>
        val targetPos = playerPos +> dir
        if !map.contains(targetPos) then false
        else if map(targetPos).field.isInstanceOf[TrapDoorField] then true
        else if map(targetPos).field.isInstanceOf[EmptyField] && player.has(1, trapDoors) then true
        else false
      }

      optDir match
        case None =>
          if map(playerPos).field.isInstanceOf[TrapDoorField] then
            player.showMessage("Tu dois te tenir à côté de la trappe pour la reprendre, et non dessus.")

        case Some(dir) =>
          val targetPos = playerPos +> dir
          if map(targetPos).field.isInstanceOf[TrapDoorField] then
            map(targetPos) += emptyField
            trapDoors.count(player) += 1
          else
            def isGround(pos: Position): Boolean =
              map(pos).field.isInstanceOf[Ground]
            val ok =
              (isGround(targetPos +> Direction.North) && isGround(targetPos +> Direction.South))
                || (isGround(targetPos +> Direction.East) && isGround(targetPos +> Direction.West))
            if ok then
              trapDoors.count(player) -= 1
              map(targetPos) += trapDoorField
            else
              player.showMessage("Impossible de placer une trappe là car il faut du sol de part et d'autre.")
  }
end TrapDoorsPlugin
