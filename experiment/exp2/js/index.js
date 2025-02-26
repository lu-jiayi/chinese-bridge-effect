///Don't touch line 2.
var order = 1;
/// Helper function that shuffles an array. Don't touch.
var shuffle = function (array) {

	var currentIndex = array.length;
	var temporaryValue, randomIndex;
	while (0 !== currentIndex) {
		randomIndex = Math.floor(Math.random() * currentIndex);
		currentIndex -= 1;
		temporaryValue = array[currentIndex];
		array[currentIndex] = array[randomIndex];
		array[randomIndex] = temporaryValue;
	}

	return array;

};

///Helper functions to get random selection from array, and to remove elements from array. Don't touch.
function getRandomNonDuplicateSelection(arr, count, exclusionArr) {
  var selected = [];
  var available = arr.filter(item => !exclusionArr.includes(item));

  if (available.length < count) {
    throw new Error('Insufficient unique elements available for selection.');
  }

  for (var i = 0; i < count; i++) {
    var randomIndex = Math.floor(Math.random() * available.length);
    selected.push(available[randomIndex]);
    available.splice(randomIndex, 1);
  }
  return selected;
}

function removeFromArray(arr, elements) {
  for (var i = 0; i < elements.length; i++) {
    var index = arr.indexOf(elements[i]);
    if (index > -1) {
      arr.splice(index, 1);
    }
  }
}

/// randomly select 2 bridge verbs and 6 unknown verbs
var bridge_verb_list = shuffle([12,13,14]).slice(0,2);
var unknown_verb_list = shuffle([1,2,3,4,5,6,7,8,9,10,11]).slice(0,6);

console.log(bridge_verb_list);
console.log(unknown_verb_list);

///add bridge verb items to the presentation list (lacking lexicalization information)
var presentation_list = [];
presentation_list.push(bridge_verb_list[0]*10 + 100002);
presentation_list.push(bridge_verb_list[0]*10 + 200001);
presentation_list.push(bridge_verb_list[0]*10 + 100001);
presentation_list.push(bridge_verb_list[0]*10 + 200002);
presentation_list.push(bridge_verb_list[1]*10 + 100001);
presentation_list.push(bridge_verb_list[1]*10 + 200002);
presentation_list.push(bridge_verb_list[1]*10 + 100002);
presentation_list.push(bridge_verb_list[1]*10 + 200001);
console.log(presentation_list);
///add  unknown verb items to the presentation list (lacking lexicalization information)
for (var i = 0; i < 6; i++){
  presentation_list.push(unknown_verb_list[i]*10 + 100002);
  presentation_list.push(unknown_verb_list[i]*10 + 200001);
  presentation_list.push(unknown_verb_list[i]*10 + 200002);
  presentation_list.push(unknown_verb_list[i]*10 + 100001);
};
console.log(presentation_list);
/// add lexicalization information to the presentation list: 1-32 each appears once.
item_number = shuffle([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32]);
for (var i = 0; i < 32; i++){
  presentation_list[i] = presentation_list[i] + (item_number[i] * 1000);
};



/// add fillers
for (var i = 1; i < 33; i++){
  presentation_list.push(900000+i);
};

console.log(presentation_list);
/// Actual experiment

///pseudorandomize: shuffle, but check if there are more than 2 critical items in a row. 
function isValid(arr) {
  let onesCount = 0;
  for (let i = 0; i < arr.length; i++) {
      if ((String(arr[i]).startsWith('1')) || (String(arr[i]).startsWith('2'))) {
          onesCount++;
          if (onesCount > 2) {
              return false;  // More than 2 numbers starting with 1 in a row
          }
      } else {
          onesCount = 0;  // Reset the counter if a number starting with 9 appears
      }
  }
  return true;  // The array satisfies the condition
}

// Repeat shuffling until the condition is met
let pseudorandomized_list;
do {
  pseudorandomized_list = shuffle(presentation_list);  // Shuffle the array
} while (!isValid(pseudorandomized_list));  // Keep shuffling until the array is valid

presentation_list = pseudorandomized_list;
console.log(presentation_list);

const populated_list = presentation_list.map((integer) => all_stimuli.find((item) => item.unique_id === integer));

console.log(populated_list);




///actual experiment
function make_slides(f) {
  var slides = {};  
  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.single_trial = slide({
    name: "single_trial",
    start: function() {
      $(".err").hide();
      $(".display_condition").html("You are in " + exp.condition + ".");
    },
    button : function() {
      response = $("#text_response").val();
      if (response.length == 0) {
        $(".err").show();
      } else {
        exp.data_trials.push({
          "trial_type" : "single_trial",
          "response" : response
        });
        exp.go(); //make sure this is at the *end*, after you log your data
      }
    },
  });

  slides.practice_slider = slide({
    name : "practice_slider",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present : [{"a": 1}],
    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      $(".err").hide();
      $(".errgood").hide();
      this.stim = stim;
      $(".prompt").html("小男孩觉得午饭很好吃。");
      this.init_sliders();
      exp.sliderPost = null; //erase current slider value
      exp.first_response_wrong = 0;
      exp.first_response_value = null;
      exp.attempts = 0;
    },
    button : function() {
      if (exp.sliderPost == null) {
        $(".err").show();
      } 
      else if (exp.sliderPost < 0.5) {
        exp.first_response_wrong = 1;
        exp.first_response_value =exp.sliderPost;
        exp.attempts = exp.attempts + 1;
        $(".errgood").show();
      }
      else {
        this.log_responses();
        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    },
    init_sliders : function() {
      utils.make_slider("#practice_slider_1", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
        "response" : exp.sliderPost,
        "first_response_value": exp.first_response_value,
        "wrong_attempts": exp.attempts,
        "item_type" : "practice_good",
        "item_number": "practice_good",
        "trial_sequence_total": 0
      });

    }
  });


  slides.post_practice_1 = slide({
    name : "post_practice_1",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });


 

  slides.practice_slider_bad = slide({
    name : "practice_slider_bad",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present : [1],

  
    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      $(".err").hide();
      $(".errbad").hide();
      $(".prompt").html("我放进去不清楚信件邮差。");
      this.init_sliders();
      exp.sliderPost = null; //erase current slider value
      exp.first_response_wrong = 0;
      exp.first_response_value = null;
      exp.attempts = 0;
    },
    button : function() {
      if (exp.sliderPost == null) {
        $(".err").show();
      } 
      else if (exp.sliderPost > 0.5) {
        exp.first_response_wrong = 1;
        exp.first_response_value = exp.sliderPost;
        exp.attempts = exp.attempts + 1;
        $(".errbad").show();
      }
      else {
        this.log_responses();
        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    },
    init_sliders : function() {
      utils.make_slider("#practice_slider_2", function(event, ui) {
        exp.sliderPost = ui.value;
        
      });
    },
    log_responses : function() {
      exp.data_trials.push({
        "response" : exp.sliderPost,
        "first_response_value": exp.first_response_value,
        "wrong_attempts": exp.attempts,
        "item_type" : "practice_bad",
        "item_number": "practice_bad",
        "trial_sequence_total": 0
      });

    }
  });

  slides.post_practice_2 = slide({
    name : "post_practice_2",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });


  slides.last_reminder = slide({
    name : "last_reminder",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
    
  });

 

  slides.one_slider = slide({
    name : "one_slider",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present : populated_list,
    
    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      $(".err").hide();
      this.stim = stim; //I like to store this information in the slide so I can record it later.
      $(".target").html(stim.sentence);
      this.init_sliders()
      exp.sliderPost = null; //erase current slider value
    },

    button : function() {
      if (exp.sliderPost == null) {
        $(".err").show();
      } else {
        this.log_responses();

        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    },

    init_sliders : function() {
      utils.make_slider("#single_slider", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },

    log_responses : function() {
      exp.data_trials.push({
        // item-specific fields
        "response" : exp.sliderPost,
        "item": this.stim.Lexicalization,
        "sentence": this.stim.sentence,
        "wh_type": this.stim.wh_type,
        "length": this.stim.Length,
        "verb": this.stim.Verb,
        "trial_sequence_total": order,
        "sentence_id": this.stim.unique_id
      });
      order = order + 1;
    }
  });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        region : $("#region").val(),
        comments : $("#comments").val(),
        fairprice: $("#fairprice").val()
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      proliferate.submit(exp.data);
    }
  });

  return slides;
}

/// init ///
function init() {
  exp.trials = [];
  exp.catch_trials = [];
  //exp.condition = _.sample(["condition 1", "condition 2"]); //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "instructions", "practice_slider", "post_practice_1", "practice_slider_bad", "post_practice_2", "last_reminder", 'one_slider', 'subj_info', 'thanks'];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    exp.go();
  });

  exp.go(); //show first slide
}
