guard :shell do
  watch(%r{.*\.cabal$}) do
    `cabal build && cabal test`
  end

  watch(%r{src/.*hs}) do
    `cabal build && cabal test`
  end

  watch(%r{test/.*hs}) do
    `cabal build && cabal test`
  end

end
