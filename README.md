## Telegram bot with Haskell

[![Build Status](https://travis-ci.org/airalab/habit.svg?branch=master)](https://travis-ci.org/airalab/habit)

### Install

    $ git clone https://github.com/airalab/habit && cd habit
    $ stack setup
    $ stack ghci

### Run your story

The `Story` is an abstraction about sparsed data getted from user
though dialogue.

```haskell
helloStory :: Story a
helloStory _ = hello <$> question "How your name?"
                     <*> question "How your surname?"
                     <*> question "How old are you?"
```

As you see the story handler `hello` is apply though the questions
to user responses.

```haskell
type Name    = Text
type Surname = Text
type Age     = Int

hello :: Monad m => Name -> Surname -> Age -> m BotMessage
hello name surname age = do
    return . toMessage $ "Hello, " <> name <> " " <> surname <> "!\n"
                      <> "You lost " <> (pack $ show age) <> " years =)"
```

To run the `Story` simple pass it to `storyBot` as value of mapping between
command an story. `APIToken` type class defines token for given platform,
e.g. Telegram platform.

```haskell
instance APIToken Telegram where
    apiToken = "bot..."

main :: IO ()
main = runBot myBot
  where myBot :: Bot Telegram ()
        myBot = storyBot helpMsg [("/hello", helloStory)]
```

Full example [text](examples/Hello.hs).
