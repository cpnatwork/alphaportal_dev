package alpha.portal.webapp.controller;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCase;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.CaseManager;

/**
 * Controller of the card form.
 * 
 * @see BaseFormController Base FormController
 */
@Controller
@RequestMapping("/cardVersionHistory*")
public class CardVersionHistoryController extends BaseFormController {

    /**
     * The CaseManager.
     * 
     * @see CaseManager CaseManager
     */
    @Autowired
    private AlphaCardManager cardManager;

    /**
     * The CaseManager.
     * 
     * @see CaseManager CaseManager
     */
    @Autowired
    private CaseManager caseManager;

    @RequestMapping(method = RequestMethod.GET)
    protected String showForm(final Model m, final HttpServletRequest request) throws IOException {

        String caseId = request.getParameter("case");
        String version = request.getParameter("version");

        String paging = "";
        for (Object e : request.getParameterMap().keySet()) {
            if (e.toString().length() > 2 && e.toString().substring(0, 2).equalsIgnoreCase("d-")) {
                paging = e.toString() + "=" + request.getParameter(e.toString());
                break;
            }
        }
        m.addAttribute("paging", paging);

        Long versionL = null;
        if (StringUtils.isNotBlank(version)) {
            try {
                versionL = Long.parseLong(version);
            } catch (NumberFormatException e) {
                saveError(request, "card.invalidVersion");
                return "redirect:/caseform?caseId=" + caseId;
            }
        }

        if (StringUtils.isBlank(caseId) || !caseManager.exists(caseId)) {
            saveError(request, "case.invalidId");
            return "redirect:/caseMenu";
        }

        AlphaCase alphaCase = caseManager.get(caseId);
        List<AlphaCard> versions = cardManager.getAllVersions(caseId);
        m.addAttribute("case", alphaCase);
        m.addAttribute("cards", versions);

        AlphaCard activeCard = null;
        User user = getUserManager().getUserByUsername(request.getRemoteUser());
        for (AlphaCard c : versions) {
            if (c.getAlphaCardIdentifier().getSequenceNumber() == versionL) {
                Adornment contributor = c.getAlphaCardDescriptor().getAdornment(AdornmentType.Contributor.getName());
                if (user == null || (contributor != null && !contributor.getValue().equals(user.getId().toString()))) {
                    saveError(request, "card.invalidId");
                    return "cardVersionHistory";
                }
                activeCard = c;
                break;
            }
        }
        m.addAttribute("activeCard", activeCard);

        return "cardVersionHistory";
    }
}
