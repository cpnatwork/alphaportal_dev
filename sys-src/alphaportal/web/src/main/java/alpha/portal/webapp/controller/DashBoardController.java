package alpha.portal.webapp.controller;

import java.util.LinkedList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.GenericManager;
import org.appfuse.service.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.ContributorRequest;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.CaseManager;

@Controller
@RequestMapping("/dashBoard*")
public class DashBoardController {

    @Autowired
    private AlphaCardManager alphaCardManager;

    @Autowired
    private UserManager userManager;

    @Autowired
    private CaseManager caseManager;

    @Autowired
    private AlphaCardManager cardManager;

    @Autowired
    private GenericManager<ContributorRequest, Long> contrReqManager;

    @RequestMapping(method = RequestMethod.GET)
    protected ModelAndView showForm(final HttpServletRequest request) {
        ModelAndView model = new ModelAndView("dashBoard");
        AlphaCard activeCard = null;

        String version = request.getParameter("version");
        Long versionL = null;
        if (StringUtils.isNotBlank(version)) {
            try {
                versionL = Long.parseLong(version);
            } catch (NumberFormatException e) {

            }
        }

        // get the current user
        User currentUser = userManager.getUserByUsername(request.getRemoteUser());

        List<AlphaCase> caseList = caseManager.findByParticipant(currentUser);

        List<AlphaCard> cardsList = cardManager.listDashBoardCards(caseList);

        for (AlphaCard c : cardsList) {
            if (c.getAlphaCardIdentifier().getSequenceNumber().equals(versionL)) {
                activeCard = c;
                break;
            }
        }

        List<ContributorRequest> contrReqList = contrReqManager.getAll();
        List<ContributorRequest> newList = new LinkedList<ContributorRequest>();

        if (!contrReqList.isEmpty()) {
            for (ContributorRequest req : contrReqList) {
                if (currentUser.getId().equals(req.getAcceptingUser().getId())) {
                    newList.add(req);
                }
            }
        }

        model.addObject("requests", newList);

        model.addObject("cards", cardsList);
        model.addObject("activeCard", activeCard);

        return model;
    }

    @RequestMapping(method = RequestMethod.POST, params = { "acceptRequest" })
    public String acceptRequest(final ContributorRequest contributorRequest, final BindingResult errors,
            final HttpServletRequest request) throws Exception {

        // get the current user
        String reqId = request.getParameter("sel");

        ContributorRequest contrReq = contrReqManager.get(Long.parseLong(reqId));
        if (contrReq == null) {
            // TODO Error
        }

        User currentUser = userManager.getUserByUsername(request.getRemoteUser());

        AlphaCard aCard = contrReq.getAlphaCard();

        aCard.getAlphaCase().addParticipant(currentUser);
        caseManager.save(aCard.getAlphaCase());
        aCard.getAlphaCardDescriptor()
                .setAdornment(AdornmentType.Contributor.getName(), currentUser.getId().toString());
        alphaCardManager.save(aCard);

        contrReqManager.remove(Long.parseLong(reqId));

        return "redirect:/dashBoard";
    }

    @RequestMapping(method = RequestMethod.POST, params = { "denyRequest" })
    public String denyRequest(final ContributorRequest contributorRequest, final BindingResult errors,
            final HttpServletRequest request) throws Exception {

        // get the current user
        String reqId = request.getParameter("sel");

        ContributorRequest contrReq = contrReqManager.get(Long.parseLong(reqId));
        if (contrReq == null) {
            // TODO Error
        }

        contrReqManager.remove(Long.parseLong(reqId));

        return "redirect:/dashBoard";
    }
}
