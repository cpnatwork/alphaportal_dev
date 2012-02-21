/**
 * 
 */
package alpha.portal.webapp.controller;

import javax.servlet.http.HttpServletRequest;

import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.service.CaseManager;

/**
 * Controller of the case menu.
 */

@Controller
@RequestMapping("/caseMenu")
public class CaseMenuController {

    /**
     * the CaseManager
     * 
     * @see CaseManager CaseManager
     */
    private CaseManager caseManager;
    /**
     * the UserManager
     * 
     * @see UserManager UserManager
     */
    private UserManager userManager;

    /**
     * sets the case manager
     * 
     * @param caseManager
     */
    @Autowired
    public void setCaseManager(@Qualifier("caseManager") CaseManager caseManager) {
        this.caseManager = caseManager;
    }

    /**
     * sets the user manager
     * @param userManager
     */
    @Autowired
    public void setUserManager(UserManager userManager) {
        this.userManager = userManager;
    }

    /**
     * handles the incoming request
     * 
     * @param request
     * 
     * @return ModelView
     * 
     * @throws Exception
     */
    @RequestMapping(method = RequestMethod.GET)
    public ModelAndView handleRequest(HttpServletRequest request) throws Exception {

        User currentUser = userManager.getUserByUsername(request.getRemoteUser());

        return new ModelAndView().addObject("caseList", caseManager.findByParticipant(currentUser));

    }
}
