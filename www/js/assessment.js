// faculty-app.js - SSM Health Faculty Evaluation System (Updated with Speech Recognition)

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

// ============================================================================
// SPEECH-TO-TEXT FUNCTIONALITY
// ============================================================================

// Check if browser supports speech recognition
function speechRecognitionSupported() {
  return 'webkitSpeechRecognition' in window || 'SpeechRecognition' in window;
}

// Initialize speech recognition for a specific textarea
function startSpeechRecognition(textareaId) {
  if (!speechRecognitionSupported()) {
    alert('Speech recognition is not supported in your browser. Please try using Chrome, Edge, or Safari.');
    return;
  }
  
  var textarea = document.getElementById(textareaId);
  if (!textarea) {
    console.error('Textarea not found:', textareaId);
    return;
  }
  
  // Create speech recognition instance
  var SpeechRecognition = window.webkitSpeechRecognition || window.SpeechRecognition;
  var recognition = new SpeechRecognition();
  
  // Configure recognition
  recognition.continuous = true;
  recognition.interimResults = true;
  recognition.lang = 'en-US';
  
  // Find the speech button for this textarea
  var speechBtn = textarea.parentNode.querySelector('.speech-btn');
  
  // Update UI to show recording state
  if (speechBtn) {
    speechBtn.innerHTML = '🔴';
    speechBtn.title = 'Recording... Click to stop';
    speechBtn.style.background = '#ffebee';
    speechBtn.style.borderColor = '#f44336';
  }
  
  // Store original text to preserve existing content
  var originalText = textarea.value;
  var finalTranscript = '';
  var interimTranscript = '';
  
  recognition.onresult = function(event) {
    interimTranscript = '';
    finalTranscript = '';
    
    for (var i = event.resultIndex; i < event.results.length; i++) {
      var transcript = event.results[i][0].transcript;
      
      if (event.results[i].isFinal) {
        finalTranscript += transcript + ' ';
      } else {
        interimTranscript += transcript;
      }
    }
    
    // Update textarea with combined text
    var newText = originalText;
    if (originalText && !originalText.endsWith(' ')) {
      newText += ' ';
    }
    newText += finalTranscript + interimTranscript;
    
    textarea.value = newText;
    
    // Trigger Shiny input update
    textarea.dispatchEvent(new Event('input', { bubbles: true }));
  };
  
  recognition.onerror = function(event) {
    console.error('Speech recognition error:', event.error);
    
    // Reset button state
    if (speechBtn) {
      speechBtn.innerHTML = '🎤';
      speechBtn.title = 'Click to use voice input';
      speechBtn.style.background = '';
      speechBtn.style.borderColor = '';
    }
    
    if (event.error === 'not-allowed') {
      alert('Microphone access was denied. Please allow microphone access and try again.');
    } else if (event.error === 'no-speech') {
      alert('No speech was detected. Please try again.');
    } else {
      alert('Speech recognition error: ' + event.error);
    }
  };
  
  recognition.onend = function() {
    // Update original text with final result
    originalText = textarea.value;
    
    // Reset button state
    if (speechBtn) {
      speechBtn.innerHTML = '🎤';
      speechBtn.title = 'Click to use voice input';
      speechBtn.style.background = '';
      speechBtn.style.borderColor = '';
    }
  };
  
  // Add click handler to stop recording
  if (speechBtn) {
    speechBtn.onclick = function() {
      if (recognition) {
        recognition.stop();
      }
    };
  }
  
  // Start recognition
  try {
    recognition.start();
  } catch (error) {
    console.error('Failed to start speech recognition:', error);
    // Reset button state
    if (speechBtn) {
      speechBtn.innerHTML = '🎤';
      speechBtn.title = 'Click to use voice input';
      speechBtn.style.background = '';
      speechBtn.style.borderColor = '';
    }
  }
}

// Initialize speech recognition buttons when page loads
document.addEventListener('DOMContentLoaded', function() {
  // Check for speech recognition support and show/hide buttons accordingly
  if (!speechRecognitionSupported()) {
    console.log('Speech recognition not supported, hiding speech buttons');
    // Hide all speech buttons if not supported
    setTimeout(function() {
      var speechBtns = document.querySelectorAll('.speech-btn');
      speechBtns.forEach(function(btn) {
        btn.style.display = 'none';
      });
    }, 1000);
  } else {
    console.log('Speech recognition supported');
  }
});

// Console logging for debugging (can be removed in production)
console.log('Faculty Evaluation App JavaScript loaded successfully');
console.log('Speech recognition supported:', speechRecognitionSupported());

// Universal refresh handler - add this to your existing assessment.js file
Shiny.addCustomMessageHandler('hideAllDynamicSections', function(data) {
  // Hide any visible dynamic sections
  $('#obs_assessment_questions_section').hide();
  $('#obs_form_controls_section').hide();
  $('#assessment_questions_section').hide();
  $('#form_controls_section').hide();
  
  // Reset any visible continue buttons
  $('#continue_to_assessment').hide();
  $('#continue_to_obs_assessment').hide();
  
  // Reset any selected buttons
  $('.obs-type-button').removeClass('selected');
  $('.eval-type-button').removeClass('selected');
  $('.clickable-quarter').removeClass('selected');
  
  console.log('All dynamic sections hidden by universal refresh');
});