class Valgrind < Formula
  desc "Dynamic analysis tools (memory, debug, profiling)"
  homepage "http://www.valgrind.org/"

  stable do
    url "https://sourceware.org/pub/valgrind/valgrind-3.14.0.tar.bz2"
    mirror "https://dl.bintray.com/homebrew/mirror/valgrind-3.14.0.tar.bz2"
    sha256 "037c11bfefd477cc6e9ebe8f193bb237fe397f7ce791b4a4ce3fa1c6a520baa5"

    depends_on maximum_macos: :high_sierra
  end

  bottle do
    sha256 high_sierra: "7869473ca1009d871dfcb496cc4d08e0318315d18721854ef42960b76e2ef64d"
    sha256 sierra:      "5ac984d472025c7bbc081e3be88b31f709944cf924945ebe85427f00d7cca73e"
  end

  head do
    url "https://github.com/LouisBrunner/valgrind-macos.git"

    depends_on "autoconf" => :build
    depends_on "automake" => :build
    depends_on "libtool" => :build
  end

  # Valgrind needs vcpreload_core-*-darwin.so to have execute permissions.
  # See #2150 for more information.
  skip_clean "lib/valgrind"

  def install
    args = %W[
      --disable-dependency-tracking
      --prefix=#{prefix}
      --enable-only64bit
      --build=amd64-darwin
    ]
    # System will autodetect where headers are located, so no need for --with-xcode-path anymore
    # Previously: https://bugs.kde.org/show_bug.cgi?id=295084

    system "./autogen.sh" if build.head?
    system "./configure", *args
    system "make"
    system "make", "install"
  end

  test do
    system "#{bin}/valgrind", "ls", "-l"
  end
end
