module API where

import Types
import Data.Vector (Vector)

statusError descr = Status {ok=False,result=Nothing,error_description=descr,error_id=Just 404,role=""} :: Types.Status News