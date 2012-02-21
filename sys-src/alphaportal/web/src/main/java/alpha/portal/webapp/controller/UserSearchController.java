package alpha.portal.webapp.controller;

import java.util.LinkedList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import alpha.portal.model.AlphaCase;
import alpha.portal.service.CaseManager;

@Controller
@RequestMapping("/userSearch*")
public class UserSearchController {

    @Autowired
    private UserManager userManager;

    @Autowired
    private CaseManager caseManager;

    @RequestMapping(method = RequestMethod.GET)
    protected void showForm() {
    }

    @RequestMapping(method = RequestMethod.POST)
    public Model onSubmit(final HttpServletRequest request, final HttpServletResponse response, final Model model)
            throws Exception {
        String userName = request.getParameter("lastName");
        String caseId = request.getParameter("case");

        if (request.getParameter("cancel") != null) {
            response.sendRedirect("caseform?caseId=" + caseId);
            return model;
        }

        if (userName != null) {
            List<User> users = userManager.getAll();
            List<User> res = new LinkedList<User>();
            for (User u : users) {
                if (u.getLastName().toLowerCase().contains(userName.toLowerCase())) {
                    res.add(u);
                }
            }
            model.addAttribute("users", res);
        }

        String[] userIds = request.getParameterValues("sel[]");

        if (ArrayUtils.isNotEmpty(userIds) && StringUtils.isNotEmpty(caseId)) {
            AlphaCase aCase = caseManager.get(caseId);

            for (String userId : userIds) {
                User participant = userManager.getUser(userId);

                aCase.addParticipant(participant);
            }
            aCase = caseManager.save(aCase);
            response.sendRedirect("caseform?caseId=" + caseId);
        }

        return model;
    }
}
