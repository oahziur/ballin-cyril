import System.Environment
import System.IO

import qualified Data.Maybe as Maybe
import qualified Data.Heap as Heap
import qualified Data.List as List
import qualified Data.Set as Set

-- Use a Max Heap and a Min Heap to track median value
updateMedian :: (Heap.MaxHeap Float, Heap.MinHeap Float, Float) ->
                Float ->
                (Heap.MaxHeap Float, Heap.MinHeap Float, Float)
updateMedian (maxHeap, minHeap, median) val
  | maxSize == 0 && minSize == 0 =
      (Heap.insert val maxHeap, minHeap, val)
  | maxSize == minSize && val < maxHead =
      let newMaxHeap = Heap.insert val maxHeap
          newMedian = getHead newMaxHeap in
      (newMaxHeap, minHeap, newMedian)
  | maxSize == minSize && val >= maxHead =
      let newMinHeap = Heap.insert val minHeap
          newMedian = getHead newMinHeap in
      (maxHeap, newMinHeap, newMedian)
  | maxSize < minSize && val < minHead =
      let newMaxHeap = Heap.insert val maxHeap
          newMedian = (getHead newMaxHeap + minHead) / 2 in
      (newMaxHeap, minHeap, newMedian)
  | maxSize < minSize && val >= minHead =
      let newMinHeap = getTail minHeap
          newMaxHeap = Heap.insert minHead maxHeap in
      updateMedian (newMaxHeap, newMinHeap, median) val
  | maxSize > minSize && val < maxHead =
      let newMaxHeap = getTail maxHeap
          newMinHeap = Heap.insert maxHead minHeap in
      updateMedian (newMaxHeap, newMinHeap, median) val 
  | maxSize > minSize && val >= maxHead =
      let newMinHeap = Heap.insert val minHeap
          newMedian = (maxHead + getHead newMinHeap) / 2 in
      (maxHeap, newMinHeap, newMedian)
  where maxSize = Heap.size maxHeap
        minSize = Heap.size minHeap

        getHead :: Heap.HeapItem pol item => Heap.Heap pol item -> item
        getHead = Maybe.fromJust . Heap.viewHead

        getTail :: Heap.HeapItem pol item => Heap.Heap pol item -> Heap.Heap pol item
        getTail = Maybe.fromMaybe Heap.empty . Heap.viewTail

        maxHead = getHead maxHeap
        minHead = getHead minHeap


generateUniqueWordsCountFromInput :: String -> [Float]
generateUniqueWordsCountFromInput = map calculateNumOfWordInTweet . separateTweets
  where separateTweets = lines
        setNub = Set.toList . Set.fromList -- remove duplicate words using Set
        calculateNumOfWordInTweet = List.genericLength . setNub . words


generateMediansFromInput :: String -> [Float]
generateMediansFromInput = extractMediansFromResult .
                           List.scanl' updateMedian (Heap.empty, Heap.empty, 0) .
                           generateUniqueWordsCountFromInput
  where extractMediansFromResult = tail . map (\(_,_,median)->median)


getOutput :: [Float] -> String
getOutput = unlines . map show

main = do
  (inputPath:outputPath:args) <- getArgs
  input <- readFile inputPath
  writeFile outputPath $ getOutput $ generateMediansFromInput input
