/**
 * 
 */
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

    /**
     * the userRolesManager
     * 
     * @see ContributorRoleManager ContributorRoleManager
     */
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
     */
    @RequestMapping(method = RequestMethod.GET)
    public ModelAndView showPage(HttpServletRequest request) throws Exception {
        Locale locale = request.getLocale();

        ModelAndView returnData = new ModelAndView();

        if (request.getParameter("delete") != null) {
            Long roleToDelete = new Long(request.getParameter("delete"));
            if (contributorRoleManager.exists(roleToDelete)) {
                try {
                    contributorRoleManager.remove(roleToDelete);
                    saveMessage(request, getText(
                            "contributorRoles.del_success", locale));
                } catch (DataIntegrityViolationException e) {
                    saveError(request, getText("contributorRoles.del_in_use",
                            locale));
                }
            } else {
                saveError(request, getText("contributorRoles.del_err", locale));
            }
        }
        if (request.getParameter("edit") != null) {
            Long roleId = new Long(request.getParameter("edit"));
            if (contributorRoleManager.exists(roleId)) {
                ContributorRole roleObj = contributorRoleManager.get(roleId);
                returnData.addObject("showEditingForm", true);
                returnData.addObject("roleToEditId", request
                        .getParameter("edit"));
                returnData.addObject("roleToEdit",
                        new String(roleObj.getName()));
            }
        }

        if (request.getParameter("edit") == null) {
            List<ContributorRole> contribList = contributorRoleManager.getAll();
            returnData.addObject("contributorRolesList", contribList);
        }

        return returnData;
    }

    /**
     * Create new role
     */
    @RequestMapping(method = RequestMethod.POST, params = { "save_new" })
    public String saveNew(HttpServletRequest request,
            HttpServletResponse response) {

        Locale locale = request.getLocale();

        String newRoleName = request.getParameter("newContributorRole");

        if (newRoleName == null || newRoleName.isEmpty()) {
            saveError(request,
                    getText("contributorRoles.add_err_empty", locale));

        } else if (contributorRoleManager.getContributorRoleByName(newRoleName) != null) {
            saveError(request, getText("contributorRoles.add_err_exists",
                    locale));

        } else {

            ContributorRole newRole = new ContributorRole(newRoleName);
            newRole = contributorRoleManager.save(newRole);
            System.out.println(newRole.toString());

            saveMessage(request,
                    getText("contributorRoles.add_success", locale));
        }

        return "redirect:/contributorRole";
    }

    /**
     * Save edited role
     */
    @RequestMapping(method = RequestMethod.POST, params = { "save_edit" })
    public String saveEdit(HttpServletRequest request,
            HttpServletResponse response) {

        Locale locale = request.getLocale();
        String urlAppend = "";

        String newRoleName = request.getParameter("newContributorRole");

        String oldRoleIdStr = request.getParameter("oldContribRoleId");
        Long oldRoleId = null;
        try {
            oldRoleId = Long.parseLong(oldRoleIdStr);
        } catch (NumberFormatException e) {
        }

        if (StringUtils.isEmpty(newRoleName)
                || StringUtils.isEmpty(oldRoleIdStr)) {

            saveError(request, getText("contributorRoles.edit_err_empty",
                    locale));

            if (oldRoleIdStr != null) {
                urlAppend = "edit=" + oldRoleIdStr;
            }

        } else if (contributorRoleManager.getContributorRoleByName(newRoleName) != null) {

            saveError(request, getText("contributorRoles.edit_err_exists",
                    locale));

            urlAppend = "edit=" + oldRoleIdStr;

        } else if (oldRoleId != null
                && !contributorRoleManager.exists(oldRoleId)) {
            saveError(request, getText("contributorRoles.edit_err_notexists",
                    locale));

        } else {
            ContributorRole editedRole = contributorRoleManager.get(oldRoleId);
            editedRole.setName(newRoleName);
            contributorRoleManager.save(editedRole);

            saveMessage(request, getText("contributorRoles.edit_success",
                    locale));

        }

        return "redirect:/contributorRole?" + urlAppend;
    }

    /**
     * 
     * @param request
     *            The http-request parameters
     * @param response
     *            Response parameters (for redirection)
     * @throws Exception
     */
    @RequestMapping(method = RequestMethod.POST)
    public String doPost(HttpServletRequest request,
            HttpServletResponse response) throws Exception {

        return "redirect:/contributorRole";
    }
}
