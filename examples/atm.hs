import Haste.HPlay.View

atm= do
   card <- waitCard
   pin <- waitPin card
   validate pin card
   setSData card
   performTransactions <|> cancel
   returnCard
   
performTransactions = do
    clearScreeen
    withdrawal <|> deposit <|> transfer  <|> balanceInquiry
    printReceipt

withdrawal= do
    wlink () "withdrawall"
    wprint "choose bank account"
    account <- chooseAccount 
    wprint "Enter the quantity"
    quantity <- getInt Nothing
    if quantity %20 /= 0 
      then do
        wprint "multiples of $20.00 please"
        stop
      else do
        r <- approbalBank account quantity
        case r of
            False -> do wprint "operation denied. sorry"
                        wprint "Another transaction?"
                        r <- wlink () "yes " <|> wlink False "No"
                        if not r then return ()
                                 else performTransactions
            True  ->  giveMoney
            
deposit= do
    wlink () "deposit"
    wprint "choose bank account"
    account <- chooseAccount 
    r <- approbalBankDeposit account
    case r of
        False -> do wprint "operation denied. sorry"
                    stop
        True  -> do
            r <- waitDeposit <|> timeout
            case r of
                False -> do wprint "timeout, sorry"; stop
                True  -> return ()
                    

validate pin card= validate' pin card 0
   where
   validate' pin card times
   r <- verifyPinBank pin card
   if r then return () else do
   if times ==2
     then do
        wprint "three tries. card will be retained"
        stop
        
     else validate pin card $ times + 1
    
switchOnOff= on <|> off
  where
  on= do
     wbutton () "On"
     wprint "enter total amount of money"
     total <- getInt Nothing
     writeMVar rtotal total
  off= do
     wbutton () "Off"
     active <- readMVar ractive
     if active then stop else wprint "ATM stopped"
        
     
        
   
