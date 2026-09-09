package org.jetbrains.plugins.scala.lang.typeInference

import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.DependencyManagerBase._
import org.jetbrains.plugins.scala.ScalaFileType
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}
import org.junit.Assert.assertNotNull

/** `props.isOpen` as a named argument of a component's props inside another component's render lambda
 * (NewRedemptionModal.scala), checked in the editor's order too: the visible reference first, the rest later.
 */
class Scala3ComponentNamedArgResolveTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_3_8)

  override protected def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+ IvyManagedLoader(
      ("com.github.japgolly.scalajs-react" %% "core_sjs1" % "4.0.0").transitive(),
      "com.lihaoyi" %% "sourcecode" % "0.4.4"
    )

  private val text =
    """import japgolly.scalajs.react.component.ScalaFn.Component
      |import japgolly.scalajs.react.vdom.all.*
      |import japgolly.scalajs.react.vdom.VdomNode
      |import japgolly.scalajs.react.{Callback, CtorType, PropsChildren, ScalaFnComponent}
      |import sourcecode.Name
      |
      |type FC[Props] = Component[Props, CtorType.Props]
      |type FCWithChildren[Props] = ScalaFnComponent[Props, CtorType.PropsAndChildren]
      |
      |object FC:
      |  def apply[P](component: P => VdomNode)(implicit name: Name): ScalaFnComponent[P, CtorType.Props] = ???
      |  def withChildren[P](component: (P, PropsChildren) => VdomNode)(implicit name: Name): ScalaFnComponent[P, CtorType.PropsAndChildren] = ???
      |
      |object Modal:
      |  final case class ModalProps(
      |    title:   VdomNode = EmptyVdom,
      |    isOpen:  Boolean = false,
      |    onClose: Option[Callback] = None
      |  )
      |  val modal: FCWithChildren[ModalProps] = FC.withChildren[ModalProps] { case (props, children) => div(children) }
      |
      |object NewRedemptionModal:
      |  final case class NewRedemptionModalProps(isOpen: Boolean, onDiscard: Callback)
      |
      |  val newRedemptionModal: FC[NewRedemptionModalProps] = FC[NewRedemptionModalProps]: props =>
      |    val seen = props.isOpen
      |    Modal.modal(Modal.ModalProps(
      |      isOpen = props.<caret>isOpen,
      |      onClose = Some(props.onDiscard),
      |      title = "New Redemption"
      |    ))(
      |      div("x")
      |    )
      |""".stripMargin

  def testWholeFileOrder(): Unit = checkTextHasNoErrors(text)

  def testVisibleReferenceFirst(): Unit = {
    myFixture.configureByText(ScalaFileType.INSTANCE, text)
    val leaf = getFile.findElementAt(getEditor.getCaretModel.getOffset)
    val ref  = PsiTreeUtil.getParentOfType(leaf, classOf[ScReferenceExpression])
    assertNotNull("no reference at caret", ref)
    assertNotNull(s"unresolved: ${ref.getText}", ref.resolve())
    myFixture.testHighlighting(false, false, false, getFile.getVirtualFile)
  }
}
