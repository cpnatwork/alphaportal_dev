package alpha.portal.webapp.controller;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.GenericManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentRules;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AdornmentTypeRange;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.ContributorRoleManager;
import alpha.portal.service.UserExtensionManager;

/**
 * Controller of the adornment form.
 * 
 * @see BaseFormController Base FormController
 */
@Controller
@RequestMapping("/adornmentform*")
public class AdornmentFormController extends BaseFormController {

    @Autowired
    private AlphaCardManager alphaCardManager;

    @Autowired
    private ContributorRoleManager contributorRoleManager;

    @Autowired
    private GenericManager<Adornment, Long> adornmentManager;

    @Autowired
    private UserExtensionManager userExtensionManager;

    /**
     * Must be called in each function of this controller to set allowed valueRange for specific AdornmentTypes.
     * 
     * @param caseId
     *            of the case of the current card
     * @param cardId
     *            of the current card
     */
    private void setupAdornmentTypes(final String caseId, final String cardId) {
        List<ContributorRole> roles = contributorRoleManager.getAll();
        List<String> roleNames = new LinkedList<String>();
        for (ContributorRole r : roles) {
            roleNames.add(r.getName());
        }
        AdornmentType.ContributorRole.setValueRange(new AdornmentTypeRange(roleNames.toArray(new String[] {})));

        AlphaCard card = alphaCardManager.get(new AlphaCardIdentifier(caseId, cardId));
        Adornment contributorRole = card.getAlphaCardDescriptor().getAdornment(AdornmentType.ContributorRole.getName());

        List<String> userIds = new LinkedList<String>();
        if (contributorRole == null || StringUtils.isBlank(contributorRole.getValue())) {
            List<User> users = getUserManager().getAll();
            for (User u : users) {
                userIds.add(u.getId().toString());
            }
        } else {
            List<UserExtension> users = userExtensionManager.getUserExtensionsByContributorRole(contributorRoleManager
                    .getContributorRoleByName(contributorRole.getValue()));
            for (UserExtension ue : users) {
                userIds.add(ue.getUserId().toString());
            }
        }
        AdornmentType.Contributor.setValueRange(new AdornmentTypeRange(userIds.toArray(new String[] {})));
    }

    @InitBinder
    public void initBinder(final WebDataBinder binder) {
        binder.setRequiredFields("name", "value");
        binder.setDisallowedFields("adornmentId");
    }

    /**
     * shows the adornment form.
     * 
     * @param request
     * 
     * @return a new adornment
     * @throws IOException
     * 
     * @see adornmentform.jsp
     */
    @RequestMapping(method = RequestMethod.GET)
    protected String showForm(final HttpServletRequest request, final Model model) {
        String adornmentId = request.getParameter("id");
        String cardId = request.getParameter("card");
        String caseId = request.getParameter("case");
        setCancelView("redirect:/caseform?activeCardId=" + cardId + "&caseId=" + caseId);
        setSuccessView("redirect:/caseform?activeCardId=" + cardId + "&caseId=" + caseId);
        setupAdornmentTypes(caseId, cardId);
        Locale locale = request.getLocale();

        List<ContributorRole> roles = contributorRoleManager.getAll();
        model.addAttribute("roles", roles);

        Adornment adornment = new Adornment();
        if (StringUtils.isNotEmpty(adornmentId)) {
            try {
                Long.valueOf(adornmentId);
            } catch (NumberFormatException e) {
                saveError(request, getText("adornment.invalidId", locale));
                model.addAttribute("adornment", adornment);
                return "redirect:/adornmentform?id=" + adornmentId + "&card=" + cardId + "&case=" + caseId;
            }
            adornment = adornmentManager.get(Long.valueOf(adornmentId));
            AdornmentType type = AdornmentType.fromName(adornment.getName());
            model.addAttribute("adornmentType", type);

        }

        AlphaCard card = alphaCardManager.get(new AlphaCardIdentifier(caseId, cardId));
        Adornment contributor = card.getAlphaCardDescriptor().getAdornment(AdornmentType.Contributor.getName());

        if (contributor.getValue() == null || contributor.getValue().isEmpty()) {

            saveError(request, getText("adornment.noAccess", locale));
            return "redirect:/caseform?activeCardId=" + cardId + "&caseId=" + caseId;

        } else {

            Long contributorID = Long.parseLong(contributor.getValue());
            User currentUser = getUserManager().getUserByUsername(request.getRemoteUser());

            if (contributorID != currentUser.getId()) {

                saveError(request, getText("adornment.noAccess", locale));
                return "redirect:/caseform?activeCardId=" + cardId + "&caseId=" + caseId;
            }
        }

        model.addAttribute("adornment", adornment);
        return null;
    }

    /**
     * handles the case, if the user clicks on one of the buttons.
     * 
     * @param newAdornment
     * 
     * @param errors
     * 
     * @param request
     * 
     * @param response
     * 
     * @return success
     * 
     * @throws Exception
     */
    @RequestMapping(method = RequestMethod.POST)
    public String onSubmit(final Adornment newAdornment, final BindingResult errors, final HttpServletRequest request,
            final HttpServletResponse response) throws Exception {
        if (request.getParameter("cancel") != null)
            return getCancelView();

        String adornmentId = request.getParameter("adornmentId");
        String cardId = request.getParameter("card");
        String caseId = request.getParameter("case");
        String success = getSuccessView();
        Locale locale = request.getLocale();
        setupAdornmentTypes(caseId, cardId);

        AlphaCardIdentifier alphaCardIdentifier = new AlphaCardIdentifier(caseId, cardId);
        AlphaCard card = alphaCardManager.get(alphaCardIdentifier);

        if (request.getParameter("delete") != null) {
            if (AdornmentType.fromName(newAdornment.getName()) == null) {
                card.getAlphaCardDescriptor().deleteAdornment(newAdornment.getName());
                saveMessage(request, getText("adornment.deleted", locale));
            } else {
                newAdornment.setValue("");
                if (AdornmentRules.applyRules(card, newAdornment)) {
                    card.getAlphaCardDescriptor().setAdornment(newAdornment);
                    saveMessage(request, getText("adornment.deleted", locale));
                } else {
                    saveError(request, getText("adornment.errorDelete", locale));
                    return getCancelView();
                }
            }

        } else {
            if (newAdornment.getName() == AdornmentType.DataProvision.getName()) {
                saveError(request, getText("adornment.errorChange", locale));
                return getCancelView();
            }
            if (AdornmentRules.applyRules(card, newAdornment)) {
                boolean isNew = card.getAlphaCardDescriptor().getAdornment(newAdornment.getName()) == null;
                card.getAlphaCardDescriptor().setAdornment(newAdornment);
                String key = (isNew) ? "adornment.added" : "adornment.updated";
                saveMessage(request, getText(key, locale));
            } else {
                saveError(request, getText("adornment.errorChange", locale));
                return getCancelView();
            }
        }

        // update Data Provision Adornment after any adornment change
        card.getAlphaCardDescriptor().setAdornment(AdornmentType.DataProvision.getName(),
                AdornmentRules.getDataProvisionStatus(card));

        if (card.getAlphaCardDescriptor().isAdornmentsChanged()) {
            card = alphaCardManager.save(card);
        }

        return success;
    }
}
