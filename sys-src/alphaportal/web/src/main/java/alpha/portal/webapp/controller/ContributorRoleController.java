/**************************************************************************
 * alpha-Portal: A web portal, for managing knowledge-driven 
 * ad-hoc processes, in form of case files.
 * ==============================================
 * Copyright (C) 2011-2012 by 
 *   - Christoph P. Neumann (http://www.chr15t0ph.de)
 *   - and the SWAT 2011 team
 **************************************************************************
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *     http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 **************************************************************************
 * $Id$
 *************************************************************************/
package alpha.portal.webapp.controller;

import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.model.ContributorRole;
import alpha.portal.service.ContributorRoleManager;

/**
 * Controller of the "Contributor Roles" menu. Show, add, edit, delete roles.
 */
@Controller
@RequestMapping("/contributorRole*")
public class ContributorRoleController extends BaseFormController {

	/** the userRolesManager. @see ContributorRoleManager ContributorRoleManager */
	@Autowired
	private ContributorRoleManager contributorRoleManager;

	/**
	 * Show list of user roles.
	 * 
	 * @param request
	 *            The http-request parameters
	 * @return Possible object names: contributorRolesList, messageId,
	 *         showEditingForm, roleToEdit
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.GET)
	public ModelAndView showPage(final HttpServletRequest request)
			throws Exception {
		final Locale locale = request.getLocale();

		final ModelAndView returnData = new ModelAndView();

		if (request.getParameter("delete") != null) {
			final Long roleToDelete = new Long(request.getParameter("delete"));
			if (this.contributorRoleManager.exists(roleToDelete)) {
				try {
					this.contributorRoleManager.remove(roleToDelete);
					this.saveMessage(request, this.getText(
							"contributorRoles.del_success", locale));
				} catch (final DataIntegrityViolationException e) {
					this.saveError(request,
							this.getText("contributorRoles.del_in_use", locale));
				}
			} else {
				this.saveError(request,
						this.getText("contributorRoles.del_err", locale));
			}
		}
		if (request.getParameter("edit") != null) {
			final Long roleId = new Long(request.getParameter("edit"));
			if (this.contributorRoleManager.exists(roleId)) {
				final ContributorRole roleObj = this.contributorRoleManager
						.get(roleId);
				returnData.addObject("showEditingForm", true);
				returnData.addObject("roleToEditId",
						request.getParameter("edit"));
				returnData.addObject("roleToEdit",
						new String(roleObj.getName()));
			}
		}

		if (request.getParameter("edit") == null) {
			final List<ContributorRole> contribList = this.contributorRoleManager
					.getAll();
			returnData.addObject("contributorRolesList", contribList);
		}

		return returnData;
	}

	/**
	 * Create new role.
	 * 
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @return the string
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "save_new" })
	public String saveNew(final HttpServletRequest request,
			final HttpServletResponse response) {

		final Locale locale = request.getLocale();

		final String newRoleName = request.getParameter("newContributorRole");

		if ((newRoleName == null) || newRoleName.isEmpty()) {
			this.saveError(request,
					this.getText("contributorRoles.add_err_empty", locale));

		} else if (this.contributorRoleManager
				.getContributorRoleByName(newRoleName) != null) {
			this.saveError(request,
					this.getText("contributorRoles.add_err_exists", locale));

		} else {

			ContributorRole newRole = new ContributorRole(newRoleName);
			newRole = this.contributorRoleManager.save(newRole);
			System.out.println(newRole.toString());

			this.saveMessage(request,
					this.getText("contributorRoles.add_success", locale));
		}

		return "redirect:/contributorRole";
	}

	/**
	 * Save edited role.
	 * 
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 * @return the string
	 */
	@RequestMapping(method = RequestMethod.POST, params = { "save_edit" })
	public String saveEdit(final HttpServletRequest request,
			final HttpServletResponse response) {

		final Locale locale = request.getLocale();
		String urlAppend = "";

		final String newRoleName = request.getParameter("newContributorRole");

		final String oldRoleIdStr = request.getParameter("oldContribRoleId");
		Long oldRoleId = null;
		try {
			oldRoleId = Long.parseLong(oldRoleIdStr);
		} catch (final NumberFormatException e) {
		}

		if (StringUtils.isEmpty(newRoleName)
				|| StringUtils.isEmpty(oldRoleIdStr)) {

			this.saveError(request,
					this.getText("contributorRoles.edit_err_empty", locale));

			if (oldRoleIdStr != null) {
				urlAppend = "edit=" + oldRoleIdStr;
			}

		} else if (this.contributorRoleManager
				.getContributorRoleByName(newRoleName) != null) {

			this.saveError(request,
					this.getText("contributorRoles.edit_err_exists", locale));

			urlAppend = "edit=" + oldRoleIdStr;

		} else if ((oldRoleId != null)
				&& !this.contributorRoleManager.exists(oldRoleId)) {
			this.saveError(request,
					this.getText("contributorRoles.edit_err_notexists", locale));

		} else {
			final ContributorRole editedRole = this.contributorRoleManager
					.get(oldRoleId);
			editedRole.setName(newRoleName);
			this.contributorRoleManager.save(editedRole);

			this.saveMessage(request,
					this.getText("contributorRoles.edit_success", locale));

		}

		return "redirect:/contributorRole?" + urlAppend;
	}

	/**
	 * Do post.
	 * 
	 * @param request
	 *            The http-request parameters
	 * @param response
	 *            Response parameters (for redirection)
	 * @return the string
	 * @throws Exception
	 *             the exception
	 */
	@RequestMapping(method = RequestMethod.POST)
	public String doPost(final HttpServletRequest request,
			final HttpServletResponse response) throws Exception {

		return "redirect:/contributorRole";
	}
}
