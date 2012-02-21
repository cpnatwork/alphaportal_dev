package alpha.portal.webapp.util;

import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;

public class AdornmentValidator implements Validator {
    private AdornmentType type;

    public AdornmentValidator(final AdornmentType t) {
        this.type = t;
    }

    public boolean supports(final Class<?> clazz) {
        return Adornment.class.equals(clazz);
    }

    public void validate(final Object target, final Errors errors) {
        Adornment a = (Adornment) target;
        if (!type.validate(a.getValue()))
            errors.rejectValue("value", "adornment.invalidValue");
    }

}
