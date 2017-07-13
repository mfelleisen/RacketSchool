#lang ql

form Box1HouseOwning {
   hasSoldHouse: "Did you sell a house in 2010?" boolean
 
   if (hasSoldHouse) {
     sellingPrice: "Price the house was sold for:" money
     privateDebt: "Private debts for the sold house:" money
     valueResidue: "Value residue:" money(sellingPrice - privateDebt)
   }
}
