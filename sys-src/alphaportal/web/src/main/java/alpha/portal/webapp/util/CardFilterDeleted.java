package alpha.portal.webapp.util;

public enum CardFilterDeleted {
    NOTDELETED("nur nicht gelöschte Cards"), DELETED("nur gelöschte Cards"), ALL(
            "alle Cards");

    private String name;

    private CardFilterDeleted(final String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
