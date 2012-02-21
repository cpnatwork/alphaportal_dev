package alpha.portal.webapp.controller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.GenericManager;
import org.appfuse.service.UserManager;
import org.hibernate.criterion.Criterion;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AdornmentTypeDeleted;
import alpha.portal.model.AdornmentTypeVisibility;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;
import alpha.portal.model.UserSession;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.CaseManager;
import alpha.portal.service.ContributorRoleManager;
import alpha.portal.service.UserExtensionManager;
import alpha.portal.service.impl.AlphaCardManagerImpl;
import alpha.portal.webapp.util.CardFilterContributor;
import alpha.portal.webapp.util.CardFilterContributorRole;
import alpha.portal.webapp.util.CardFilterDataProvision;
import alpha.portal.webapp.util.CardFilterDeleted;
import alpha.portal.webapp.util.CardFilterHolder;

/**
 * Controller of the case form.
 * 
 * @see BaseFormController Base FormController
 */
@Controller
@RequestMapping("/caseform*")
public class CaseFormController extends BaseFormController {

    private static final String CARD_PRIORITY_PARAM_NAME = "cardPriority";

    /**
     * The CaseManager.
     * 
     * @see CaseManager CaseManager
     */
    @Autowired
    private CaseManager caseManager;

    /**
     * The UserManager.
     * 
     * @see UserManager UserManager
     */
    @Autowired
    private UserManager userManager;

    @Autowired
    private UserExtensionManager userExtensionManager;

    @Autowired
    private ContributorRoleManager contributorRoleManager;

    /**
     * the AlphaCardManager.
     * 
     * @see AlphaCardManager AlphaCardManager
     */
    @Autowired
    private AlphaCardManager alphaCardManager;

    /**
     * The UserSessionManager
     */
    @Autowired
    private GenericManager<UserSession, Long> userSessionManager;

    /**
     * Default constructor, who sets the Cancel- and the Success-View
     */
    public CaseFormController() {
        setCancelView("redirect:/caseMenu");
        setSuccessView("redirect:/caseMenu");
    }

    /**
     * shows the case form
     * 
     * @param request
     * 
     * @param response
     * 
     * @return ModelView
     * 
     * @throws Exception
     * 
     * @see caseform.jsp
     */
    @ModelAttribute("activeCard")
    @RequestMapping(method = RequestMethod.GET)
    protected ModelAndView showForm(final CardFilterHolder filters, final HttpServletRequest request,
            final HttpServletResponse response) throws Exception {
        AlphaCard activeCard = null;
        User currentUser = null;
        String caseId = request.getParameter("caseId");
        String activeCardId = request.getParameter("activeCardId");

        ModelAndView m = new ModelAndView("caseform");

        if (request.getParameter("isMyWorklist") != null) {
            m.addObject("isMyWorklist", true);
            filters.setContributor(CardFilterContributor.OWN);
            filters.setDataProvision(CardFilterDataProvision.NOTFULFILLED);

            filters.setContributorRole(CardFilterContributorRole.ALL);
            filters.setShowDeleted(CardFilterDeleted.NOTDELETED);
        }

        /**
         * Merge filters with Session
         */
        filters.mergeFiltersWithSession(request, response);

        if (!StringUtils.isBlank(caseId) && (caseId.equals("last") || caseManager.exists(caseId))) {

            currentUser = userManager.getUserByUsername(request.getRemoteUser());
            UserSession userSession;
            if (userSessionManager.exists(currentUser.getId())) {
                userSession = userSessionManager.get(currentUser.getId());
            } else {
                userSession = new UserSession();
                userSession.setUserId(currentUser.getId());
            }
            m.addObject("currentUserId", currentUser.getId());

            AlphaCase apCase = null;
            // show last viewed case
            if (caseId.equals("last")) {
                String lastCaseId = userSession.getLastViewedCaseId();
                if (StringUtils.isBlank(lastCaseId) || !caseManager.exists(lastCaseId)) {
                    // redirect to list
                    response.sendRedirect("caseMenu");
                } else {
                    apCase = caseManager.get(lastCaseId);
                }

            } else {
                apCase = caseManager.get(caseId);
                if (apCase != null && !StringUtils.isBlank(apCase.getCaseId())) {
                    userSession.setLastViewedCaseId(apCase.getCaseId());
                    userSessionManager.save(userSession);
                }
            }

            m.addObject("case", apCase);
            if (apCase != null) {
                setSuccessView("redirect:/caseform?caseId=" + apCase.getCaseId());
                m.addObject("cards", filterAlphaCards(apCase, filters, currentUser));

                m.addObject("participants", apCase.getListOfParticipants());

                AlphaCardIdentifier activeCardIdentifier = new AlphaCardIdentifier(caseId, activeCardId);
                if (!StringUtils.isBlank(activeCardId)) {

                    if (alphaCardManager.exists(activeCardIdentifier)) {
                        activeCard = alphaCardManager.get(activeCardIdentifier);
                        m.addObject("activeCard", activeCard);

                        Adornment deletedAdornment = activeCard.getAlphaCardDescriptor().getAdornment(
                                AdornmentType.Deleted.getName());
                        if (deletedAdornment != null) {
                            if (deletedAdornment.getValue().equals(AdornmentTypeDeleted.TRUE.value())) {
                                m.addObject("activeCardIsDeleted", true);
                            }
                        }

                        boolean hidePayload = false;

                        Adornment contrbitorAdornment = activeCard.getAlphaCardDescriptor().getAdornment(
                                AdornmentType.Contributor.getName());

                        Adornment visibilityAdornment = activeCard.getAlphaCardDescriptor().getAdornment(
                                AdornmentType.Visibility.getName());

                        if (contrbitorAdornment != null && visibilityAdornment != null) {
                            String cId = contrbitorAdornment.getValue();
                            String vis = visibilityAdornment.getValue();

                            if (cId != null && !cId.isEmpty()) {

                                Long contributorID = Long.parseLong(activeCard.getAlphaCardDescriptor().getAdornment(
                                        AdornmentType.Contributor.getName()).getValue());

                                if (vis.equals(AdornmentTypeVisibility.PRIVATE.value()) && contributorID != null
                                        && !(currentUser.getId() == contributorID)) {

                                    hidePayload = true;
                                }
                            } else {

                                if (activeCard.getAlphaCardDescriptor()
                                        .getAdornment(AdornmentType.Visibility.getName()).getValue().equals(
                                                AdornmentTypeVisibility.PRIVATE.value())) {

                                    hidePayload = true;
                                }
                            }
                        }

                        m.addObject("hidePayload", hidePayload);

                        boolean currentUserMatchesContributorRole = false;
                        if (StringUtils.isBlank(activeCard.getAlphaCardDescriptor().getContributorRole())) {
                            currentUserMatchesContributorRole = true;
                        }
                        if (!currentUserMatchesContributorRole) {
                            ContributorRole role = contributorRoleManager.getContributorRoleByName(activeCard
                                    .getAlphaCardDescriptor().getContributorRole());
                            if (role == null) {
                                currentUserMatchesContributorRole = true;
                            } else if (userExtensionManager.exists(currentUser.getId())) {
                                UserExtension ue = userExtensionManager.get(currentUser.getId());
                                if (ue != null) {
                                    currentUserMatchesContributorRole = ue.hasRole(role);
                                }
                            }
                        }
                        m.addObject("currentUserMatchesContributorRole", currentUserMatchesContributorRole);
                        m.addObject("currentUserIsContributor", currentUser.getId() == activeCard
                                .getAlphaCardDescriptor().getContributor());

                        // new gui stuff
                        Set<String> userRoleStrings = new HashSet<String>();
                        if (userExtensionManager.exists(currentUser.getId())) {
                            Set<ContributorRole> UserRoles = userExtensionManager.get(currentUser.getId()).getRoles();
                            for (ContributorRole contributorRole : UserRoles) {
                                userRoleStrings.add(contributorRole.getName());
                            }
                        }
                        m.addObject("currentUserContributorRoles", userRoleStrings.toArray(new String[] {}));

                        setSuccessView("redirect:/caseform?caseId=" + apCase.getCaseId() + "&activeCardId="
                                + activeCardIdentifier.getCardId());

                    } else if (activeCardId.equals("new")) {
                        m.addObject("activeCard", alphaCardManager.createAlphaCard(caseId));

                    }
                }

            }

            // Filters
            m.addObject("filters", filters);

            // Essential Adornments
            List<String> essential = new LinkedList<String>();
            essential.add(AdornmentType.Title.getName());
            essential.add(AdornmentType.Contributor.getName());
            essential.add(AdornmentType.ContributorRole.getName());
            m.addObject("essentialAdornments", essential.toArray(new String[] {}));

        } else {
            m.addObject("case", new AlphaCase());
        }
        return m;
    }

    public List<AlphaCard> filterAlphaCards(final AlphaCase apCase, final CardFilterHolder filters, final User user) {

        CardFilterContributor contributor = filters.getContributor();
        CardFilterContributorRole contributorRole = filters.getContributorRole();
        CardFilterDataProvision dataProvision = filters.getDataProvision();
        CardFilterDeleted showDeleted = filters.getShowDeleted();

        UserExtension userExtension = null;
        try {
            userExtension = userExtensionManager.get(user.getId());
        } catch (Exception e) {
            userExtension = new UserExtension();
        }

        List<Criterion> list = new ArrayList<Criterion>();
        Set<String> setStr = new HashSet<String>();

        Set<ContributorRole> set = userExtension.getRoles();

        for (ContributorRole tmp : set) {
            setStr.add(tmp.getName());
        }

        if (contributor != null) {
            switch (contributor) {
            case OTHERS:

                list.add(AlphaCardManagerImpl.getContributorCriterionOthers(user.getId().toString()));
                break;
            case OWN:

                list.add(AlphaCardManagerImpl.getContributorCriterionOwn(user.getId().toString()));

            default:
                break;
            }
        }

        if (contributorRole != null && !setStr.isEmpty()) {
            switch (contributorRole) {
            case OTHERS:

                list.add(AlphaCardManagerImpl.getContributorRoleCriterionOthers(setStr.toArray(new String[0])));
                setStr.add("");
                break;
            case OWN:

                list.add(AlphaCardManagerImpl.getContributorRoleCriterionOwn(setStr.toArray(new String[0])));
            default:
                break;
            }
        } else if (contributorRole != null && setStr.isEmpty()) {
            setStr.add("0");
        }

        if (dataProvision != null) {
            switch (dataProvision) {
            case FULLFILLED:

                list.add(AlphaCardManagerImpl.DATA_PROVISION_FULFILLED);
                break;
            case INPROGRESS:

                list.add(AlphaCardManagerImpl.DATA_PROVISION_INPROGRESS);
                break;
            case NOTFULFILLED:

                list.add(AlphaCardManagerImpl.DATA_PROVISION_OPEN_INPROGRESS);
                break;
            case OPEN:

                list.add(AlphaCardManagerImpl.DATA_PROVISION_OPEN);

            default:
                break;
            }
        }

        if (dataProvision != null) {
            switch (dataProvision) {
            case FULLFILLED:

                list.add(AlphaCardManagerImpl.DATA_PROVISION_FULFILLED);
                break;
            case INPROGRESS:

                list.add(AlphaCardManagerImpl.DATA_PROVISION_INPROGRESS);
                break;
            case NOTFULFILLED:

                list.add(AlphaCardManagerImpl.DATA_PROVISION_OPEN_INPROGRESS);
                break;
            case OPEN:

                list.add(AlphaCardManagerImpl.DATA_PROVISION_OPEN);

            default:
                break;
            }
        }

        if (showDeleted == null) {
            list.add(AlphaCardManagerImpl.NOT_DELETED);
        } else if (showDeleted == CardFilterDeleted.NOTDELETED) {
            list.add(AlphaCardManagerImpl.NOT_DELETED);
        } else if (showDeleted == CardFilterDeleted.DELETED) {
            list.add(AlphaCardManagerImpl.DELETED);
        }

        if (list.isEmpty())
            return apCase.getAlphaCards();
        else
            return alphaCardManager.listAlphaCardsByCriterion(apCase.getCaseId(), list.toArray(new Criterion[0]));
    }

    /**
     * handles the case, if the user clicks on one of the buttons.
     * 
     * @param myCase
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
    @SuppressWarnings("unchecked")
    @RequestMapping(method = RequestMethod.POST)
    public String onSubmit(final AlphaCase myCase, final BindingResult errors, final HttpServletRequest request,
            final HttpServletResponse response) throws Exception {

        log.fatal("This fallback Method should not be called");

        Enumeration params = request.getParameterNames();
        while (params.hasMoreElements()) {
            log.error(params.nextElement().toString());
        }

        return "redirect:/caseform?caseId=" + myCase.getCaseId();
    }

    @RequestMapping(method = RequestMethod.POST, params = { "addCase" })
    public String addCase(AlphaCase alphaCase, final BindingResult errors, final HttpServletRequest request,
            final HttpServletResponse response) throws Exception {

        User currentUser = userManager.getUserByUsername(request.getRemoteUser());
        alphaCase.addParticipant(currentUser);
        alphaCase = caseManager.save(alphaCase);

        saveMessage(request, getText("case.added", request.getLocale()));
        return "redirect:/caseform?caseId=" + alphaCase.getCaseId();

    }

    @RequestMapping(method = RequestMethod.POST, params = { "saveCase" })
    public String saveCase(final AlphaCase jspCase, final BindingResult errors, final HttpServletRequest request,
            final HttpServletResponse response) throws Exception {

        AlphaCase alphaCase = caseManager.get(jspCase.getCaseId());

        if (!StringUtils.isBlank(jspCase.getName())) {
            alphaCase.setName(jspCase.getName());
            alphaCase = caseManager.save(alphaCase);
        }

        String cardPriority = request.getParameter(CARD_PRIORITY_PARAM_NAME);
        if (!StringUtils.isBlank(cardPriority)) {
            caseManager.updateCardOrder(alphaCase, Arrays.asList(StringUtils.split(cardPriority, ';')));
        }

        saveMessage(request, getText("case.updated", request.getLocale()));
        String redirect = "redirect:/caseform?caseId=" + alphaCase.getCaseId();
        String activeCardId = request.getParameter("activeCardId");
        if (!StringUtils.isBlank(activeCardId)) {
            redirect += "&activeCardId=" + activeCardId;
        }
        return redirect;
    }

    @RequestMapping(method = RequestMethod.POST, params = { "cancelCase" })
    public String cancelCase(final AlphaCase alphaCase, final BindingResult errors, final HttpServletRequest request)
            throws Exception {
        return getCancelView();
    }

    @RequestMapping(method = RequestMethod.POST, params = { "deleteCase" })
    public String deleteCase(final AlphaCase alphaCase, final BindingResult errors, final HttpServletRequest request)
            throws Exception {
        caseManager.remove(alphaCase.getCaseId());
        saveMessage(request, getText("case.deleted", request.getLocale()));
        return getCancelView();
    }

}
