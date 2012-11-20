stackage
========

"Stable Hackage," tools for creating a vetted set of packages from Hackage.

A note about the codebase: the goal is to minimize dependencies and have
the maximum range of supported compiler versions. Therefore, we avoid
anything "complicated." For example, instead of using the text package,
we use Strings everywhere.
