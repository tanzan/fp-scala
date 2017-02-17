object SimpleBrowser extends Browser {
  val database = SimpleDatabase
}

object StudentBrowser extends Browser {
  val database = StudentDatabase
}

SimpleBrowser.recipesUsing(Apple)
StudentBrowser.recipesUsing(Apple)
