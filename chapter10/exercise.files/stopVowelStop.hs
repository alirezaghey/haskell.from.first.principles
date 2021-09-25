import Distribution.Types.GenericPackageDescription.Lens (_Impl)
import Distribution.Types.Lens (_Impl)
stops = "pbtdkg"
vowels = "aeiou"

create3Tuples :: String -> String -> [(Char, Char, Char)]
create3Tuples s v = [(a, b, c) | a <- stops, b <- vowels, c <- stops]


create3TuplesStartingWithP :: String -> String -> [(Char, Char, Char)]
create3TuplesStartingWithP s v = [('p', b, c) | b <- v, c <- s]

verbs = ["Act","Answer","Approve","Arrange","Break","Build","Buy","Coach"
        ,"Color","Cough","Create","Complete","Cry","Dance","Describe","Draw"
        ,"Drink","Eat","Edit","Enter","Exit","Imitate","Invent","Jump","Laugh"
        ,"Lie","Listen","Paint","Plan","Play","Read","Replace","Run","Scream"
        ,"See","Shop","Shout","Sing","Skip","Sleep","Sneeze","Solve","Study"
        ,"Teach","Touch","Turn","Walk","Win","Write","Whistle","Yank","Zip"]
nouns = ["Actor","Gold","Painting","Advertisement","Grass","Parrot","Afternoon"
        ,"Greece","Pencil","Airport","Guitar","Piano","Ambulance","Hair","Pillow"
        ,"Animal","Hamburger","Pizza","Answer","Helicopter","Planet","Apple"
        ,"Helmet","Plastic","Army","Holiday","Portugal","Australia","Honey","Potato"
        ,"Balloon","Horse","Queen","Banana","Hospital","Quill","Battery","House"
        ,"Rain","Beach","Hydrogen","Rainbow","Beard","Ice","Raincoat","Bed","Insect"
        ,"Refrigerator","Belgium","Insurance","Restaurant","Boy","Iron","River"
        ,"Branch","Island","Rocket","Breakfast","Jackal","Room","Brother","Jelly"
        ,"Rose","Camera","Jewellery","Russia","Candle","Jordan","Sandwich","Car"
        ,"Juice","School","Caravan","Kangaroo","Scooter","Carpet","King","Shampoo"
        ,"Cartoon","Kitchen","Shoe","China","Kite","Soccer","Church","Knife","Spoon"
        ,"Crayon","Lamp","Stone","Crowd","Lawyer","Sugar","Daughter","Leather"
        ,"Sweden","Death","Library","Teacher","Denmark","Lighter","Telephone"
        ,"Diamond","Lion","Television","Dinner","Lizard","Tent","Disease","Lock"
        ,"Thailand","Doctor","London","Tomato","Dog","Lunch","Toothbrush","Dream"
        ,"Machine","Traffic","Dress","Magazine","Train","Easter","Magician","Truck"
        ,"Egg","Manchester","Uganda","Eggplant","Market","Umbrella","Egypt","Match"
        ,"Van","Elephant","Microphone","Vase","Energy","Monkey","Vegetable","Engine"
        ,"Morning","Vulture","England","Motorcycle","Wall","Evening","Nail","Whale"
        ,"Eye","Napkin","Window","Family","Needle","Wire","Finland","Nest","Xylophone"
        ,"Fish","Nigeria","Yacht","Flag","Night","Yak","Flower","Notebook","Zebra"
        ,"Football","Ocean","Zoo","Forest","Oil","Garden","Fountain","Orange","Gas"
        ,"France","Oxygen","Girl","Furniture","Oyster","Glass","Garage","Ghost"]


createNounVerbNounTuples :: [String] -> [String] -> [(String, String, String)]
createNounVerbNounTuples n v = [(a, b, c) | a <- nouns, b <- verbs, c <- nouns]