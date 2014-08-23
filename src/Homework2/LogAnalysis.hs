{-# LANGUAGE OverloadedStrings #-}

module Homework2.LogAnalysis where

import Homework2.Log

parseMessage :: String -> LogMessage
parseMessage string = case words string of
  ("I":infoNumber:message) ->
    LogMessage Info (read infoNumber) $ unwords message
  ("E":severity:infoNumber:message) ->
    LogMessage (Error $ read severity) (read infoNumber) $ unwords message
  ("W":infoNumber:message) ->
    LogMessage Warning (read infoNumber) $ unwords message
  otherwise -> Unknown string

parse :: String -> [LogMessage]
parse = map parseMessage . lines

