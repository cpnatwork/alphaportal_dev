package alpha.portal.webapp.util;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class CardFilterHolder {
    private CardFilterDataProvision dataProvision;
    private CardFilterContributorRole contributorRole;
    private CardFilterContributor contributor;
    private CardFilterDeleted deleted;

    /**
     * @return the dataProvision
     */
    public CardFilterDataProvision getDataProvision() {
        return dataProvision;
    }

    /**
     * @param dataProvision
     *            the dataProvision to set
     */
    public void setDataProvision(final CardFilterDataProvision dataProvision) {
        this.dataProvision = dataProvision;
    }

    /**
     * @return the contributorRole
     */
    public CardFilterContributorRole getContributorRole() {
        return contributorRole;
    }

    /**
     * @param contributorRole
     *            the contributorRole to set
     */
    public void setContributorRole(
            final CardFilterContributorRole contributorRole) {
        this.contributorRole = contributorRole;
    }

    /**
     * @return the contributor
     */
    public CardFilterContributor getContributor() {
        return contributor;
    }

    /**
     * @param contributor
     *            the contributor to set
     */
    public void setContributor(final CardFilterContributor contributor) {
        this.contributor = contributor;
    }

    /**
     * @return Show Deleted?
     */
    public CardFilterDeleted getShowDeleted() {
        return deleted;
    }

    /**
     * 
     * @param deleted
     *            Set: show deleted?
     */
    public void setShowDeleted(CardFilterDeleted deleted) {
        this.deleted = deleted;
    }

    /**
     * 
     * @param request
     */
    public void mergeFiltersWithSession(HttpServletRequest request,
            HttpServletResponse response) {
        String cookieValDataProvision = null;
        String cookieValContributorRole = null;
        String cookieValContributor = null;
        String cookieValDeleted = null;

        Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (final Cookie thisCookie : cookies) {
                if (thisCookie.getName().equals("aCardFilterDataProvision")) {
                    cookieValDataProvision = thisCookie.getValue();
                } else if (thisCookie.getName().equals(
                        "aCardFilterContributorRole")) {
                    cookieValContributorRole = thisCookie.getValue();
                } else if (thisCookie.getName()
                        .equals("aCardFilterContributor")) {
                    cookieValContributor = thisCookie.getValue();
                } else if (thisCookie.getName().equals("aCardFilterDeleted")) {
                    cookieValDeleted = thisCookie.getValue();
                }
            }
        }

        boolean isOk;

        if (dataProvision == null) {
            if (cookieValDataProvision != null) {
                isOk = false;
                for (CardFilterDataProvision compare : CardFilterDataProvision
                        .values()) {
                    if (cookieValDataProvision.equals(compare.name())) {
                        isOk = true;
                        dataProvision = compare;
                        break;
                    }
                }

                if (!isOk) {
                    dataProvision = CardFilterDataProvision.ALL;
                }
            } else if (cookieValDataProvision == null) {
                dataProvision = CardFilterDataProvision.ALL;
            }
        }

        if (contributorRole == null) {
            if (cookieValContributorRole != null) {
                isOk = false;
                for (CardFilterContributorRole compare : CardFilterContributorRole
                        .values()) {
                    if (cookieValContributorRole.equals(compare.name())) {
                        isOk = true;
                        contributorRole = compare;
                        break;
                    }
                }

                if (!isOk) {
                    contributorRole = CardFilterContributorRole.ALL;
                }
            } else if (cookieValContributorRole == null) {
                contributorRole = CardFilterContributorRole.ALL;
            }
        }

        if (contributor == null) {
            if (cookieValContributor != null) {
                isOk = false;
                for (CardFilterContributor compare : CardFilterContributor
                        .values()) {
                    if (cookieValContributor.equals(compare.name())) {
                        isOk = true;
                        contributor = compare;
                        break;
                    }
                }

                if (!isOk) {
                    contributor = CardFilterContributor.ALL;
                }
            } else if (cookieValContributor == null) {
                contributor = CardFilterContributor.ALL;
            }
        }

        if (deleted == null) {
            if (cookieValDeleted != null) {
                isOk = false;
                for (CardFilterDeleted compare : CardFilterDeleted.values()) {
                    if (cookieValDeleted.equals(compare.name())) {
                        isOk = true;
                        deleted = compare;
                        break;
                    }
                }

                if (!isOk) {
                    deleted = CardFilterDeleted.NOTDELETED;
                }
            } else if (cookieValDeleted == null) {
                deleted = CardFilterDeleted.NOTDELETED;
            }
        }

        Cookie cookie = new Cookie("aCardFilterDataProvision", dataProvision
                .name());
        cookie.setSecure(false);
        cookie.setPath("/");
        cookie.setMaxAge(3600 * 24 * 30);
        response.addCookie(cookie);

        cookie = new Cookie("aCardFilterContributorRole", contributorRole
                .name());
        cookie.setSecure(false);
        cookie.setPath("/");
        cookie.setMaxAge(3600 * 24 * 30);
        response.addCookie(cookie);

        cookie = new Cookie("aCardFilterContributor", contributor.name());
        cookie.setSecure(false);
        cookie.setPath("/");
        cookie.setMaxAge(3600 * 24 * 30);
        response.addCookie(cookie);

        cookie = new Cookie("aCardFilterDeleted", deleted.name());
        cookie.setSecure(false);
        cookie.setPath("/");
        cookie.setMaxAge(3600 * 24 * 30);
        response.addCookie(cookie);

        return;
    }
}
