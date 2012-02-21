package alpha.portal.webapp.controller;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.GenericManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataAccessException;
import org.springframework.orm.ObjectRetrievalFailureException;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.ContributorRequest;
import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.ContributorRoleManager;
import alpha.portal.service.UserExtensionManager;

@Controller
@RequestMapping("/cardassignform*")
public class CardAssignFormController extends BaseFormController {
    @Autowired
    private UserExtensionManager userExtensionManager;

    @Autowired
    private AlphaCardManager cardManager;

    @Autowired
    private ContributorRoleManager contributorRoleManager;

    @Autowired
    private GenericManager<ContributorRequest, Long> contrReqManager;

    /**
     * Handles the viewing of all users who have the card's ContributorRole.
     */
    @RequestMapping(method = RequestMethod.GET)
    protected void showForm(final HttpServletRequest request, final Model model) {
        String caseId = request.getParameter("case");
        String cardId = request.getParameter("card");
        AlphaCard card = null;
        try {
            card = cardManager.get(new AlphaCardIdentifier(caseId, cardId));
        } catch (ObjectRetrievalFailureException e) {
            saveError(request, getText("cardassign.noCard", request.getLocale()));
        }
        if (card == null)
            return;

        Adornment contributorRole = card.getAlphaCardDescriptor().getAdornment(AdornmentType.ContributorRole.getName());
        if (contributorRole == null) {
            saveError(request, getText("cardassign.noContributorRole", request.getLocale()));
            return;
        }

        ContributorRole role = contributorRoleManager.getContributorRoleByName(contributorRole.getValue());
        List<UserExtension> users = new LinkedList<UserExtension>();
        if (role == null || StringUtils.isBlank(role.getName())) {
            List<User> list = getUserManager().getAll();
            for (User u : list) {
                users.add(new UserExtension(u));
            }
        } else {
            users = userExtensionManager.getUserExtensionsByContributorRole(role);
        }

        model.addAttribute("users", users);
    }

    private void returnToCase(final HttpServletResponse response, final String caseId, final String cardId)
            throws IOException {
        response.sendRedirect("caseform?caseId=" + caseId + "&activeCardId=" + cardId);
    }

    /**
     * Adds the selected user to the case's participants and sets him as Contributor for the card. Also returns to
     * caseform on cancel.
     */
    @RequestMapping(method = RequestMethod.POST)
    public void onSubmit(final HttpServletRequest request, final HttpServletResponse response) throws Exception {
        String caseId = request.getParameter("case");
        String cardId = request.getParameter("card");
        String userId = request.getParameter("user");

        if (request.getParameter("cancel") != null) {
            returnToCase(response, caseId, cardId);
            return;
        }

        if (StringUtils.isBlank(userId)) {
            saveError(request, getText("cardassign.noUser", request.getLocale()));
            returnToCase(response, caseId, cardId);
            return;
        }

        Long userIdLong = null;
        try {
            userIdLong = Long.parseLong(userId);
        } catch (NumberFormatException e) {
            saveError(request, "cardassign.invalidUser");
            returnToCase(response, caseId, cardId);
            return;
        }

        if (userIdLong == null) {
            saveMessage(request, getText("cardassign.invalidUser", request.getLocale()));
            returnToCase(response, caseId, cardId);
            return;
        }

        AlphaCard card = cardManager.get(new AlphaCardIdentifier(caseId, cardId));

        UserExtension ue = null;
        try {
            ue = userExtensionManager.get(userIdLong);
        } catch (DataAccessException e) {
        }
        if (ue == null) {
            try {
                User u = getUserManager().get(userIdLong);
                ue = new UserExtension(u);
                ue = userExtensionManager.save(ue);
            } catch (DataAccessException e) {
                saveError(request, "cardassign.invalidUser");
                returnToCase(response, caseId, cardId);
                return;
            }
        }

        ContributorRequest contrRequest = new ContributorRequest(getUserManager().getUserByUsername(
                request.getRemoteUser()), getUserManager().get(userIdLong), card);
        contrRequest = contrReqManager.save(contrRequest);

        response.sendRedirect("caseform?caseId=" + caseId + "&activeCardId=" + cardId);
    }
}
