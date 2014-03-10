import Data.Char

convert letter = (toLower letter, toUpper letter, ord letter)

order a b c d | a > b = order b a c d
              | b > c = order a c b d
              | c > d = order a b d c
              | otherwise = (a, b, c, d)

type DayInTheYear = (Int, Int, Int)

day :: DayInTheYear -> Int
day (d, m, y) = d

month :: DayInTheYear -> Int
month (d, m, y) = m

year :: DayInTheYear -> Int
year (d, m, y) = y

daysBetween :: DayInTheYear -> DayInTheYear -> Int
daysBetween d1 d2 = 365 * (year d2 - year d1) +
                    30 * (month d2 - month d1) +
                    (day d2 - day d1)

-- Teacher = ( id, name, degree, sex )
type Teacher = (Int, String, String, Char)

base :: Int -> Teacher
base x | x == 0  = (1793, "Pedro Paulo",      	     "MASTER", 'M')
       | x == 1  = (1797, "Joana Silva",      	     "MASTER", 'F')
       | x == 2  = (1534, "João de Medeiros", 	     "DOCTOR", 'M')
       | x == 3  = (1267, "Claudio Cesar de Sa",     "DOCTOR", 'M')
       | x == 4  = (1737, "Paula de Medeiros",       "MASTER", 'F')
       | x == 5  = (1888, "Rita de Matos",           "MASTER", 'F')
       | x == 6  = (1698, "Tereza Cristina Andrade", "MASTER", 'F')
       | x == 7  = (0,    "",                        "",       'O')

degree :: Teacher -> String
degree (a, b, c, d) = c

isDoctor (a, b, "DOCTOR", d) = True
isDoctor _ = False

countIfDoctor 7 = 0
countIfDoctor x | isDoctor (base x) = 1 + countIfDoctor (x + 1)
                | otherwise = countIfDoctor (x + 1)

amountOfDoctors = countIfDoctor 0

isWoman (a, b, c, 'F') = True
isWoman _ = False

countIfWoman 7 = 0
countIfWoman x | isWoman (base x) = 1 + countIfWoman (x + 1)
               | otherwise = countIfWoman (x + 1)

amountOfWomen = countIfWoman 0

isMan (a, b, c, 'M') = True
isMan _ = False

isMaster (a, b, "MASTER", d) = True
isMaster _ = False

countIfManAndMaster 7 = 0
countIfManAndMaster x 
             | isMan (base x) && isMaster (base x) = 1 + countIfManAndMaster (x + 1)
             | otherwise = countIfManAndMaster (x + 1)

amountOfMasterMen = countIfManAndMaster 0

getId (a, b, c, d) = a

minorId :: Int -> Int
minorId 7 = 9999
minorId x | getId (base x) < minorId (x + 1) = getId (base x)
          | otherwise = minorId (x + 1)

minor = minorId 0
