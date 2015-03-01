package controllers

import models.Tables._
import models.Tables.profile.simple._
import play.api.Logger
import play.api.data.Forms._
import play.api.data._
import play.api.db.slick.{DBAction, _}
import play.api.mvc.Controller

object UserController extends Controller {

  //フォームからの入力値
  case class UserForm(id: Option[Long], name: String, companyId: Option[Int])

  val userForm = Form(
    mapping(
      "id"        -> optional(longNumber),
      "name"      -> nonEmptyText(maxLength = 20),
      "companyId" -> optional(number)
    )(UserForm.apply)(UserForm.unapply)
  )

  /**
   * 一覧表示
   */
  def list = DBAction { implicit rs =>
    val users = Users.sortBy(t => t.id).list
    Ok(views.html.user.list(users))
  }

  /**
   * 登録・編集画面表示
   */
  def edit(id: Option[Long]) = DBAction { implicit rs =>
    // リクエストパラメータにIDが存在する場合
    val form = if(id.isDefined) {
      // idをキーにしてユーザー情報を１件取得
      val user = Users.filter(_.id === id.get.bind).first
      Logger.debug(user.toString)
      // 値をフォームにつめる
      userForm.fill(UserForm(Some(user.id),user.name,user.companyId))
    } else {
      //リクエストパラメータにidが存在しない場合はカラのフォーム
      userForm
    }
    // 会社一覧を取得
    val companies = Companies.sortBy(t => t.id).list
    Ok(views.html.user.edit(form,companies))
  }

  /**
   * 登録実行
   */
  def create = DBAction.transaction {implicit  rs =>
    //httpリクエストの内容をuserFormにバインド
    userForm.bindFromRequest.fold(
      // 入力値にエラーが有る場合は登録画面へ
      error => BadRequest(views.html.user.edit(error, Companies.sortBy(t => t.id).list)),
      // 入力が正しければ登録して一覧がめんへ
      form => {
        val user = UsersRow(0,form.name,form.companyId)
        Users.insert(user)
        Redirect(routes.UserController.list)
      }
    )
  }

  /**
   * 更新実行
   */
  def update = DBAction.transaction {implicit  rs =>
    //httpリクエストの内容をuserFormにバインド
    userForm.bindFromRequest.fold(
      // 入力値にエラーが有る場合は登録画面へ
      error => BadRequest(views.html.user.edit(error, Companies.sortBy(t => t.id).list)),
      // 入力が正しければ登録して一覧がめんへ
      form => {
        val user = UsersRow(form.id.get,form.name,form.companyId)
        Users.filter(_.id === user.id.bind).update(user)
        Redirect(routes.UserController.list)
      }
    )
  }

  /**
   * 削除実行
   */
  def remove(id: Long) = DBAction.transaction { implicit rs =>
    Users.filter(_.id === id.bind).delete
    Redirect(routes.UserController.list)
  }

}
