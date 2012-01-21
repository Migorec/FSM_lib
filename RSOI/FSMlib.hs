{-# LANGUAGE MultiParamTypeClasses #-}

module RSOI.FSMlib where

-- | Typeclass for finite state machine Класс коннечного автомата
--
-- Parameters: Параметры:
--
-- * FSM state Состояние автомата (s)
--
-- * request state (d)
--
-- * messages (m)
--
-- * timers (t)

class (Eq s, Eq m, Eq t) => FSM s d m t where
    -- | State-transition function. Функция перехода автомата. 
    state :: s -- ^ previous FSM state предыдущее состояние автомата
          -> m -- ^ message recieved полученное сообщение
          -> d -- ^ previuos request state предыдущее состояние заявки
          -> (s, d) -- ^ new FSM and rewuest state новое состояние автомата и заявки
    
    -- | Function defining timers which should be started when entering new state. Функция определяющая таймеры, которые необходимо запустить при переходе в новое состояние
    -- Часть функции выхода
    timeout :: s -- ^ new FSM state новое состояние автомата
            -> [(t,Int)] -- ^ list of pairs (timer,time) список пар (таймер, время)
            
    -- | Output function. Функция выхода автомата
    action :: s -- ^ new FSM state новое состояние автомата
           -> m -- ^ message that changed state сообщение, по которому был выполнен переход
           -> d -- ^ request state состояние заявки
           -> IO d -- ^ new request state + side effects (such as sending message to another system) новое состояние заявки + побочные эффекты (отправка сообщений другим системам, к примеру)
    