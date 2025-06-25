// faculty-app.js - SSM Health Faculty Evaluation System

// Enhanced validation and modal handling
Shiny.addCustomMessageHandler('showValidationWarning', function(data) {
  var warningDiv = document.getElementById('faculty_validation_warning');
  var fieldsList = document.getElementById('missing_fields_list');
  
  if (data.show) {
    fieldsList.innerHTML = data.fields;
    warningDiv.style.display = 'block';
    // Smooth scroll to warning in modal
    warningDiv.scrollIntoView({ behavior: 'smooth', block: 'center' });
  } else {
    warningDiv.style.display = 'none';
  }
});

// Handle successful faculty addition
Shiny.addCustomMessageHandler('closeFacultyModal', function(data) {
  var modal = document.getElementById('addFacultyModal');
  if (modal) {
    // Use Bootstrap 5 Modal API
    var modalInstance = bootstrap.Modal.getInstance(modal);
    if (modalInstance) {
      modalInstance.hide();
    }
  }
});

// Handle modal reset
Shiny.addCustomMessageHandler('resetFacultyForm', function(data) {
  // Hide validation warning when modal opens
  var warningDiv = document.getElementById('faculty_validation_warning');
  if (warningDiv) {
    warningDiv.style.display = 'none';
  }
});

// Add event listener for modal events
document.addEventListener('DOMContentLoaded', function() {
  var modal = document.getElementById('addFacultyModal');
  if (modal) {
    modal.addEventListener('shown.bs.modal', function() {
      // Reset form when modal opens
      Shiny.setInputValue('reset_faculty_form', Math.random());
    });
    
    modal.addEventListener('hidden.bs.modal', function() {
      // Clean up when modal closes
      var warningDiv = document.getElementById('faculty_validation_warning');
      if (warningDiv) {
        warningDiv.style.display = 'none';
      }
    });
  }
});

// Enhanced form validation with visual feedback
function validateFacultyForm() {
  var requiredFields = ['fac_name', 'fac_email', 'fac_div', 'fac_fell'];
  var missingFields = [];
  
  requiredFields.forEach(function(fieldId) {
    var field = document.getElementById(fieldId);
    var formGroup = field ? field.closest('.form-group') : null;
    
    if (!field || !field.value || field.value.trim() === '') {
      missingFields.push(fieldId);
      if (formGroup) {
        formGroup.classList.add('has-error');
      }
    } else {
      if (formGroup) {
        formGroup.classList.remove('has-error');
      }
    }
  });
  
  return missingFields;
}

// Real-time validation (optional enhancement)
function addRealTimeValidation() {
  var requiredFields = ['fac_name', 'fac_email', 'fac_div', 'fac_fell'];
  
  requiredFields.forEach(function(fieldId) {
    var field = document.getElementById(fieldId);
    if (field) {
      field.addEventListener('blur', function() {
        var formGroup = field.closest('.form-group');
        if (formGroup) {
          if (field.value && field.value.trim() !== '') {
            formGroup.classList.remove('has-error');
          } else {
            formGroup.classList.add('has-error');
          }
        }
      });
      
      field.addEventListener('input', function() {
        var formGroup = field.closest('.form-group');
        if (formGroup && field.value && field.value.trim() !== '') {
          formGroup.classList.remove('has-error');
        }
      });
    }
  });
}

// Initialize real-time validation when document is ready
document.addEventListener('DOMContentLoaded', function() {
  // Small delay to ensure Shiny has rendered the form
  setTimeout(addRealTimeValidation, 500);
});

// Email validation helper
function validateEmail(email) {
  var emailPattern = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/;
  return emailPattern.test(email);
}

// Add visual feedback for email validation
document.addEventListener('DOMContentLoaded', function() {
  setTimeout(function() {
    var emailField = document.getElementById('fac_email');
    if (emailField) {
      emailField.addEventListener('blur', function() {
        var formGroup = emailField.closest('.form-group');
        if (formGroup) {
          if (emailField.value && !validateEmail(emailField.value)) {
            formGroup.classList.add('has-error');
          } else if (emailField.value && validateEmail(emailField.value)) {
            formGroup.classList.remove('has-error');
          }
        }
      });
    }
  }, 500);
});

// Loading state management
function setButtonLoading(buttonId, isLoading) {
  var button = document.getElementById(buttonId);
  if (button) {
    if (isLoading) {
      button.disabled = true;
      button.classList.add('loading');
      button.innerHTML = button.innerHTML + ' <span class="spinner-border spinner-border-sm" role="status" aria-hidden="true"></span>';
    } else {
      button.disabled = false;
      button.classList.remove('loading');
      // Remove spinner
      var spinner = button.querySelector('.spinner-border');
      if (spinner) {
        spinner.remove();
      }
    }
  }
}

// Console logging for debugging (can be removed in production)
console.log('Faculty Evaluation App JavaScript loaded successfully');