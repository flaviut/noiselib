package factorygame

import com.badlogic.gdx.backends.lwjgl.{LwjglApplication, LwjglApplicationConfiguration}
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.{Texture, Pixmap, GL20}
import com.badlogic.gdx.{ApplicationAdapter, Gdx}
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import scala.util.Random
import factorygame.util.{Logger, Color}

object Main extends App {
	new LwjglApplication(
		App,
		new LwjglApplicationConfiguration()
	)
}

object App extends ApplicationAdapter {
	lazy val pixmap: Pixmap = new Pixmap(512, 512, Pixmap.Format.RGBA8888)
	lazy val batch: SpriteBatch = new SpriteBatch()
	lazy val shape: ShapeRenderer = new ShapeRenderer()
	Logger.addLogger(println)

	override def create() = {
		for (x <- 0 until pixmap.getWidth) {
			for (y <- 0 until pixmap.getHeight) {
				pixmap.drawPixel(x, y, {
					val col = Color(
						Random.nextFloat(),
						Random.nextFloat(),
						Random.nextFloat())
					Logger.logInfo(col.toString)
					col.rgba
				})
			}
		}
	}

	override def render() = {
		Gdx.gl.glClearColor(0, 0, 0, 1)
		Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT)
		batch.begin()
		val tex = new Texture(pixmap, Pixmap.Format.RGBA8888, false)
		batch.draw(tex, 0, 0)
		batch.end()
		tex.dispose()
	}
}
