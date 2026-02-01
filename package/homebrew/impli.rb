class Impli < Formula
  desc "Interpreter for an imperative toy language"
  homepage "https://github.com/bfeitknecht/impli"
  url "https://github.com/bfeitknecht/impli/archive/refs/tags/v4.0.0.0.tar.gz"
  sha256 ""
  license "MIT"

  depends_on "ghc@9.10" => :build
  depends_on "cabal-install" => :build

  def install
    system "cabal", "v2-update"
    system "cabal", "v2-install", "--install-method=copy", "--installdir=#{bin}"
    man1.install "docs/man/impli.1"
  end

  test do
    assert_match "impli", shell_output("#{bin}/impli --version")
  end
end
