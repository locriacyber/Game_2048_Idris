module GameLoop

import Effects
import System
import Gamelogic
import Types

gameLoop : Fuel -> {n,m : Nat } -> Matrix n m Int -> Eff [PlayerInput, DisplayInfo, Random Int] ()
gameLoop Dry _ = pure ()
gameLoop (More fuel) state = do
   displayState state
   case gameWinOver state 2048 of
      GameWin => do info "Congratulations! You win the game!"
      GameOver => do info "Sorry! You lose the game!"
      GameContinue => do
         info "The game continues!"
         case !getInput of
            Movement direction => do
               let newstate = moves direction state
               let False = newstate == state
               | True => gameLoop fuel state
               let num = !randomInt
               let pos = !randomInt
               newmat <- pure (addNum newstate (twoFour num) (posAvail pos state))
               gameLoop fuel newmat
            Quit => do
               info "Exiting!"
            Invalid => do
               info "The right operation is 'wasd'! "
               gameLoop fuel state

export
main_pure : Fuel -> Eff [PlayerInput, DisplayInfo, Random Int] ()
main_pure fuel = do
   info "Welcome to 2048 game!"
   startMat <- initMat 4 4
   gameLoop fuel startMat
   info "Game end"
