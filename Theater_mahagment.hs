import Data.Maybe (fromMaybe)
import Data.List (nub, find)

-- Movie data structure
data Movie = Movie {
    movieId :: Int,
    title :: String,
    showtime :: String,
    ticketPrice :: Double
} deriving (Show)

-- Seat data structure
data Seat = Seat {
    seatId :: Int,
    row :: Char,
    number :: Int,
    availability :: Bool  -- True if available, False if booked
} deriving (Show)

-- Booking data structure
data Booking = Booking {
    bookingId :: Int,
    movie :: Movie,
    bookedSeats :: [Seat],  -- List of seats booked
    totalCost :: Double,    -- Total cost after discounts
    customerEmail :: String
} deriving (Show)

-- Representing a Promo Code and its discount value
type PromoCode = String
type Discount = Double

-- A map of promo codes to their discounts
promoCodes :: [(PromoCode, Discount)]
promoCodes = [("AITU", 0.10), ("WENEED80FORFINAL)", 0.80), ("HASKELL", 0.05)]

-- Book Seats for a Movie
bookSeats :: Movie -> [Seat] -> [Seat] -> PromoCode -> String -> Either String Booking
bookSeats movie availableSeats selectedSeats promoCode email
    | any (not . availability) selectedSeats = Left "Some selected seats are already booked!"
    | not (all (elem availableSeats) selectedSeats) = Left "Some selected seats do not exist in this theater!"
    | otherwise = Right Booking {
        bookingId = generateBookingId,
        movie = movie,
        bookedSeats = selectedSeats,
        totalCost = calculateTotalCost selectedSeats promoCode,
        customerEmail = email
    }
  where
    -- Generate a unique booking ID
    generateBookingId = 1  -- A placeholder, should be generated dynamically

    -- Calculate the total cost based on the ticket price and promo code
    calculateTotalCost selectedSeats promoCode =
        let baseCost = fromIntegral (length selectedSeats) * ticketPrice movie
            discount = fromMaybe 0 (lookup promoCode promoCodes)
        in baseCost * (1 - discount)

-- Cancel Booking
cancelBooking :: [Booking] -> Int -> [Seat] -> Either String [Booking]
cancelBooking bookings bookingId seatsToRelease =
    case find (\b -> bookingId == bookingId b) bookings of
        Nothing -> Left "Booking not found!"
        Just booking -> Right (map releaseSeats bookings)
  where
    releaseSeats booking
        | bookingId booking == bookingId = booking { bookedSeats = releasedSeats }
        | otherwise = booking
    releasedSeats = map (\s -> s { availability = True }) (bookedSeats booking)

-- Check Seat Availability
checkSeatAvailability :: Movie -> [Seat] -> [Seat] -> Bool
checkSeatAvailability _ availableSeats selectedSeats =
    all (\s -> s elem availableSeats && availability s) selectedSeats

-- Apply Promotional Code
applyPromoCode :: PromoCode -> [Seat] -> Movie -> Double
applyPromoCode promoCode selectedSeats movie =
    let baseCost = fromIntegral (length selectedSeats) * ticketPrice movie
        discount = fromMaybe 0 (lookup promoCode promoCodes)
    in baseCost * (1 - discount)

-- Generate Movie Theater Statistics

-- Total revenue per movie
totalRevenue :: [Booking] -> Int -> Double
totalRevenue bookings movieId =
    sum [totalCost booking | booking <- bookings, movieId (movie booking) == movieId]

-- Total seats sold per movie
totalSeatsSold :: [Booking] -> Int -> Int
totalSeatsSold bookings movieId =
    sum [length (bookedSeats booking) | booking <- bookings, movieId (movie booking) == movieId]

-- Average booking cost per movie
averageBookingCost :: [Booking] -> Int -> Double
averageBookingCost bookings movieId =
    let relevantBookings = [booking | booking <- bookings, movieId (movie booking) == movieId]
        total = sum [totalCost booking | booking <- relevantBookings]
        count = length relevantBookings
    in if count == 0 then 0 else total / fromIntegral coun
