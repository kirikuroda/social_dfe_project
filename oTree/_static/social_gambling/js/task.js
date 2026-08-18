"use strict;"

//////////////////////////////////
//--------Task parameters-------//
//////////////////////////////////

// From Python
const condition = js_vars.condition;
let nPlayers = js_vars.n_active_players;
let totalRoundNumber;
if (condition === "practice") {
    totalRoundNumber = 2;
} else {
    totalRoundNumber = 40;
}
const position = js_vars.position;
const roundNumber = js_vars.round_number;
const maxSample = js_vars.max_sample;
const defaultPosition = "ab";
if (position === defaultPosition) {
    leftNumSeq = JSON.parse(js_vars.seq_a);
    rightNumSeq = JSON.parse(js_vars.seq_b);
} else {
    leftNumSeq = JSON.parse(js_vars.seq_b);
    rightNumSeq = JSON.parse(js_vars.seq_a);
}
leftNumSeq = leftNumSeq.map(num => num.toFixed(1));
rightNumSeq = rightNumSeq.map(num => num.toFixed(1));

// Stimuli
const optionDodge = 250;
const optionSize = 180;
const centerX = window.innerWidth / 2; // The x-coordinate of center of the aperture on the screen, in pixels
const centerY = window.innerHeight / 2; // The y-coordinate of center of the aperture on the screen, in pixels
const socialSize = optionSize * 0.45; // Relative size of the social information
const socialMargin = socialSize * 0.15;
const socialDodge = optionSize * 0.25;
const textSize = 40;
const font = `${textSize}pt Roboto`;
const textBaseline = "middle";
const textAlign = "center";
const leftOptionX = centerX - optionDodge - optionSize / 2;
const rightOptionX = centerX + optionDodge - optionSize / 2;
const optionY = centerY - optionSize / 2;
const leftTextX = centerX - optionDodge;
const rightTextX = centerX + optionDodge;
const textY = optionY + optionSize / 2;
const defaultLineWidth = 4;
const image = new Image();
image.src = "/static/social_gambling/img/social.png";

// Fixation-cross and color parameters
const defaultColor = "white";
const alertColor = "yellow";
const fixationSize = 20; //The width of the fixation  in pixels; 0.5 degs
const fixationThickness = 2; //The thickness of the fixation , must be positive number above 1

// Time-limit, duration
const timeLimitSample = 15200; // milliseconds = 15 seconds
const sampleDuration = 3000;
const iti = 1000;
const feedbackDuration = 1000; // milliseconds
const alertDuration = 5000; // milliseconds
const alphaDuration = 50; // milliseconds
const alphaDelay = feedbackDuration - alphaDuration; // milliseconds

// Keys
const leftKey = "ArrowLeft";
const rightKey = "ArrowRight";
const leftClick = "leftClick";
const rightClick = "rightClick";
const optionA = "a";
const optionB = "b";

//////////////////////////////////////
//--------Set up Canvas begin-------//
//////////////////////////////////////

//Create a canvas element and append it to the DOM
let canvas = document.createElement("canvas");
let body = document.body;
document.body.appendChild(canvas);

//Remove the margins and paddings of the display_element
body.style.margin = 0;
body.style.padding = 0;
body.style.backgroundColor = "black";

//Remove the margins and padding of the canvas
canvas.style.margin = 0;
canvas.style.padding = 0;

// use absolute positioning in top left corner to get rid of scroll bars
canvas.style.position = "absolute";
canvas.style.top = 0;
canvas.style.left = 0;
// canvas.style.cursor = "none";

// Get the context of the canvas so that it can be painted on.
let ctx = canvas.getContext("2d");
const dpr = window.devicePixelRatio;
const canvasWidth = canvas.width = window.innerWidth * dpr;
const canvasHeight = canvas.height = window.innerHeight * dpr;
ctx.scale(dpr, dpr);
canvas.style.width = `${window.innerWidth}px`;
canvas.style.height = `${window.innerHeight}px`;

/////////////////////////////////////// 
// Define the data set for the trial //
///////////////////////////////////////

// Initialize object to store the response data.
// Default values of -99 are used if the trial times out and the subject has not pressed a valid key
let response = {
    sample: [],
    rt_sample: [],
    feedback_time_sample: [],
    choice_self: "miss",
    rt_self: -99,
    frame_self: -99,
    choice_other: [],
    rt_other: [],
    frame_other: [],
    is_dropout_self: true,
    is_dropout_other: true,
    n_active_players: nPlayers
}

if (condition === "personal") {
    response.is_dropout_other = false;
}


///////////////////////////////////////////////
// Task functions (keyboard, recording etc.) //
///////////////////////////////////////////////

// Detect the key press
function keyPressEvent(e) {
    keyCode = e.code;
    // Reject repeated and multiple key presses
    if (!e.repeat && !isChosenSelf && frameCounter < maxSample) {
        // Sampling
        if (!isSampledSelf && (keyCode === leftKey || keyCode === rightKey)) {
            sampleOption(keyCode);
        }
    }
    return false;
}

function mouseClickEvent(e) {
    // Mouse position
    const rect = canvas.getBoundingClientRect();
    const clickX = e.clientX - rect.left;
    const clickY = e.clientY - rect.top;
    if (!isSampledSelf && !isChosenSelf && isSampledBoth) {
        if (
            clickX >= leftOptionX && clickX <= leftOptionX + optionSize &&
            clickY >= optionY && clickY <= optionY + optionSize
        ) {
            chooseOption(leftClick);
        } else if (
            clickX >= rightOptionX && clickX <= rightOptionX + optionSize &&
            clickY >= optionY && clickY <= optionY + optionSize
        ) {
            chooseOption(rightClick);
        }
    }
}

function mouseMoveEvent(e) {
    // Mouse position
    const rect = canvas.getBoundingClientRect();
    const clickX = e.clientX - rect.left;
    const clickY = e.clientY - rect.top;
    if (
        clickX >= leftOptionX && clickX <= leftOptionX + optionSize &&
        clickY >= optionY && clickY <= optionY + optionSize
    ) {
        isHoverLeft = true;
        isHoverRight = false;
    } else if (
        clickX >= rightOptionX && clickX <= rightOptionX + optionSize &&
        clickY >= optionY && clickY <= optionY + optionSize
    ) {
        isHoverLeft = false;
        isHoverRight = true;
    } else {
        isHoverLeft = false;
        isHoverRight = false;
    }
}


// Convert the key/click to the corresponding option
function LR2option(LR) {
    if (position === defaultPosition) {
        if (LR === leftKey || LR === leftClick) {
            return optionA;
        } else {
            return optionB;
        }
    } else {
        if (LR === leftKey || LR === leftClick) {
            return optionB;
        } else {
            return optionA;
        }
    }
}

// Convert the option to the corresponding key
function option2position(option) {
    if (position === defaultPosition) {
        if (option === optionA) {
            return "left";
        } else {
            return "right";
        }
    } else {
        if (option === optionA) {
            return "right";
        } else {
            return "left";
        }
    }
}

function sampleOption(key) {
    responseTime = performance.now()
    response.rt_sample.push(Math.round(responseTime - startTimeTrial));
    response.is_dropout_self = false;
    isSampledSelf = true;
    pressedKey = key;
    response.sample.push(LR2option(pressedKey));
    if (key === leftKey && !isSampledLeft) {
        isSampledLeft = true;
    } else if (key === rightKey && !isSampledRight) {
        isSampledRight = true;
    }
    
    // Wait for 200ms
    setTimeout(() => {
        if (condition === "social") {
            liveSend({choice: "sampled", sample_id: frameCounter});
            // Reset late flag immediately after sending to prevent "please wait" message
            if (nSampledPlayers[frameCounter] + nChosenPlayers[frameCounter] === nPlayers - 1) {
                isLateOthers = false;
                // If this is the last player to sample, set isSampledPlayers immediately
                isSampledPlayers = true;
            }
        }
    }, 200);
}

function checkSampledBoth(flgLeft, flgRight) {
    if (flgLeft && flgRight) {
        return true;
    }
}

function chooseOption(click) {
    clickedKey = click;
    responseTime = performance.now()
    response.rt_self = Math.round(responseTime - startTimeTrial);
    response.frame_self = frameCounter + 1; // Number of samples, not index
    response.choice_self = LR2option(click);
    response.is_dropout_self = false;
    isChosenSelf = true;
    if (condition === "social") {
        setTimeout(() => {
            // Reset late flag immediately after sending to prevent "please wait" message
            if (nChosenPlayers[frameCounter] === nPlayers - 1) {
                isChosenSelf = true;
                isChosenPlayers = true;
                isLateOthers = false;
            } else if (nSampledPlayers[frameCounter] + nChosenPlayers[frameCounter] === nPlayers - 1) {
                isLateOthers = false;
            }
                liveSend({choice: response.choice_self, sample_id: frameCounter});
        }, 200);
    }
}

function liveRecv(data) {
    response.is_dropout_other = false;
    choice = data.choice;
    sample_id = data.sample_id;
    if (choice === optionA || choice === optionB) {
        response.choice_other.push(choice);
        nChosenPlayers[sample_id] = data.n_chosen_players;
    } else if (choice === "chosen") {
        nChosenPlayers[sample_id] = data.n_chosen_players;
    } else if (choice === "sampled") {
        nSampledPlayers[sample_id] = data.n_sampled_players;
    }
    if (nChosenPlayers[sample_id] === nPlayers) {
        isChosenPlayers = true;
        isLateOthers = false;
    } else if (nSampledPlayers[frameCounter] + nChosenPlayers[frameCounter] === nPlayers) {
        isSampledPlayers = true;
        isLateOthers = false;
    }
}

function recordData(response) {
    $("<input>").attr({
        type: "hidden",
        name: "taskdata",
        value: JSON.stringify(response)
    }).appendTo("#form");
    $("#form").submit();
}

function endTrial() {
    isRoundCompleted = true;
    if (nPlayers === 1) {
        response.is_dropout_other = true;
    }
    response.n_active_players = nPlayers;
    recordData(response);
    body.innerHTML = "";
}

// End if all the players finished the trial
function endAllPlayers() {
    if (isChosenSelf && isChosenPlayers) {
        setTimeout(endTrial, feedbackDuration);
    }
}

///////////////////////
// Drawing functions //
///////////////////////

function drawFixation() {
    //Horizontal line
    ctx.clearRect(0, 0, canvasWidth, canvasHeight);
    ctx.beginPath();
    ctx.lineWidth = fixationThickness;
    ctx.moveTo(centerX - fixationSize / 2, optionY + optionSize / 2);
    ctx.lineTo(centerX + fixationSize / 2, optionY + optionSize / 2);
    ctx.strokeStyle = defaultColor;
    ctx.stroke();
    //Vertical line
    ctx.lineWidth = fixationThickness;
    ctx.moveTo(centerX, optionY + optionSize / 2 - fixationSize / 2);
    ctx.lineTo(centerX, optionY + optionSize / 2 + fixationSize / 2);
    ctx.strokeStyle = defaultColor;
    ctx.stroke();
}

function drawRoundNumber(roundNumber, totalRoundNumber) {
    ctx.textBaseline = textBaseline;
    ctx.textAlign = textAlign;
    ctx.font = font;
    ctx.fillStyle = defaultColor;
    ctx.fillText(`Round ${roundNumber}/${totalRoundNumber}`, centerX, optionY + optionSize / 2);
}

function drawOption() {
    ctx.beginPath();
    ctx.textBaseline = textBaseline;
    ctx.textAlign = textAlign;

    // Mouse cursor
    if ((isHoverLeft || isHoverRight) && !isSampledBoth) {
        canvas.style.cursor = "not-allowed";
    } else {
        canvas.style.cursor = "default";
    }

    if (!isSampledSelf && !isChosenSelf) {
        // Draw option boxes
        if (isHoverLeft && isSampledBoth) {
            ctx.strokeStyle = `rgba(13,110,253, ${alpha})`;
            ctx.lineWidth = defaultLineWidth * 3;
            ctx.strokeRect(leftOptionX, optionY, optionSize, optionSize);
            ctx.strokeStyle = "rgba(255,255,255," + alpha + ")";
            ctx.lineWidth = defaultLineWidth;
            ctx.strokeRect(rightOptionX, optionY, optionSize, optionSize);
        } else if (isHoverRight && isSampledBoth) {
            ctx.strokeStyle = "rgba(255,255,255," + alpha + ")";
            ctx.lineWidth = defaultLineWidth;
            ctx.strokeRect(leftOptionX, optionY, optionSize, optionSize);
            ctx.strokeStyle = `rgba(13,110,253, ${alpha})`;
            ctx.lineWidth = defaultLineWidth * 3;
            ctx.strokeRect(rightOptionX, optionY, optionSize, optionSize);
        } else if (!isSampledBoth){
            ctx.strokeStyle = "rgba(108,117,125," + alpha + ")";
            ctx.lineWidth = defaultLineWidth;
            ctx.strokeRect(leftOptionX, optionY, optionSize, optionSize);
            ctx.strokeRect(rightOptionX, optionY, optionSize, optionSize);
        } else {
            ctx.strokeStyle = "rgba(255,255,255," + alpha + ")";
            ctx.lineWidth = defaultLineWidth;
            ctx.strokeRect(leftOptionX, optionY, optionSize, optionSize);
            ctx.strokeRect(rightOptionX, optionY, optionSize, optionSize);
        }
    } else if (isSampledSelf) {
        // After sampling
        if (pressedKey === leftKey) {
            ctx.lineWidth = defaultLineWidth * 3;
            ctx.strokeStyle = `rgba(13,110,253, ${alpha})`;
            ctx.strokeRect(leftOptionX, optionY, optionSize, optionSize);
            ctx.lineWidth = defaultLineWidth;
            if (isSampledBoth) {
                ctx.strokeStyle = "rgba(255,255,255," + alpha + ")";
            } else {
                ctx.strokeStyle = "rgba(108,117,125," + alpha + ")";
            }
            ctx.strokeRect(rightOptionX, optionY, optionSize, optionSize);
        } else {
            ctx.lineWidth = defaultLineWidth;
            if (isSampledBoth) {
                ctx.strokeStyle = "rgba(255,255,255," + alpha + ")";
            } else {
                ctx.strokeStyle = "rgba(108,117,125," + alpha + ")";
            }
            ctx.strokeRect(leftOptionX, optionY, optionSize, optionSize);
            ctx.lineWidth = defaultLineWidth * 3;
            ctx.strokeStyle = `rgba(13,110,253, ${alpha})`;
            ctx.strokeRect(rightOptionX, optionY, optionSize, optionSize);
        }
    } else if (isChosenSelf) {
        // Draw option boxes
        if (clickedKey === leftClick) {
            ctx.fillStyle = `rgba(13,110,253, ${alpha})`;
            ctx.strokeStyle = `rgba(13,110,253, ${alpha})`;
            ctx.lineWidth = defaultLineWidth * 3;
            ctx.fillRect(leftOptionX, optionY, optionSize, optionSize);
            ctx.strokeRect(leftOptionX, optionY, optionSize, optionSize);
            ctx.strokeStyle = "rgba(255,255,255," + alpha + ")";
            ctx.lineWidth = defaultLineWidth;
            ctx.strokeRect(rightOptionX, optionY, optionSize, optionSize);
        } else if (clickedKey === rightClick) {
            ctx.strokeStyle = "rgba(255,255,255," + alpha + ")";
            ctx.lineWidth = defaultLineWidth;
            ctx.strokeRect(leftOptionX, optionY, optionSize, optionSize);
            ctx.fillStyle = `rgba(13,110,253, ${alpha})`;
            ctx.strokeStyle = `rgba(13,110,253, ${alpha})`;
            ctx.lineWidth = defaultLineWidth * 3;
            ctx.fillRect(rightOptionX, optionY, optionSize, optionSize);
            ctx.strokeRect(rightOptionX, optionY, optionSize, optionSize);
        }
    }
    
    // Show the amount of reward after sampling
    if (((isSampledSelf || isChosenSelf) && (isSampledPlayers || isChosenPlayers))) {
        ctx.font = font;
        ctx.fillStyle = defaultColor;
        if (!isChosenSelf) {
            if (pressedKey === leftKey) {
                ctx.fillText(leftNumSeq[frameCounter], leftTextX, textY);
            } else {
                ctx.fillText(rightNumSeq[frameCounter], rightTextX, textY);
            }
        }
        if (!isFeedbackStarted) {
            startTimeFeedback = performance.now();
            response.feedback_time_sample.push(Math.round(startTimeFeedback - startTimeTrial));
            isFeedbackStarted = true;
        }
        // Social information
        socialInfoPosition = [];
        for (const element of response.choice_other) {
            socialInfoPosition.push(option2position(element));
        }
    }

    // Social information
    drawSocial(socialInfoPosition);
    
    // Prompt the response
    if (frameCounter == maxSample && !isChosenSelf && !isTimer) {
        drawMessage(messageType = "maxsamples");
    } else if ((!isSampledSelf && !isChosenSelf) && isTimeout && !isTimer) {
        drawMessage(messageType = "alert");
    } else if (condition === "social" && isChosenSelf && !isChosenPlayers) {
        drawMessage(messageType = "wait_everyone");
    } else if (condition === "social" && isLateOthers && (isSampledSelf || isChosenSelf) && (!isSampledPlayers && !isChosenPlayers)) {
        drawMessage(messageType = "wait");
    }
    ctx.stroke();
    ctx.fill();
}

function drawMessage(messageType) {
    ctx.textBaseline = textBaseline;
    ctx.textAlign = textAlign;
    ctx.font = `${textSize * 3 / 4}pt Roboto`;
    if (messageType === "alert") {
        text = "Please try or play a lottery now!";
        ctx.fillStyle = alertColor;
    } else if (messageType === "wait") {
        text = "Please wait until everyone tries or plays a lottery.";
        ctx.fillStyle = "rgba(255,255,255," + alpha + ")";
    } else if (messageType === "wait_everyone") {
        text = "Please wait until everyone plays a lottery.";
        ctx.fillStyle = "white";
    } else if (messageType === "maxsamples") {
        text = "Due to time constraints, you cannot try anymore.";
        ctx.fillStyle = alertColor;
    }
    ctx.fillText(text, centerX, centerY + optionSize);
    if (messageType === "maxsamples") {
        text = "Please play a lottery now!";
        ctx.fillStyle = alertColor;
        ctx.fillText(text, centerX, centerY + optionSize * 1.5);
    }
}

function drawSocial(socialInfoPosition) {
    nLeft = 0;
    nRight = 0;
    if (response.choice_other.length >= 1 && condition === "social") {
        for (const element of socialInfoPosition) {
            if (element === "left") {
                nLeft += 1;
                if (nLeft % 2 === 1) {
                    leftColumn = -1;
                } else {
                    leftColumn = 1;
                }
                personX = centerX - optionDodge - socialSize / 2 + socialDodge * leftColumn;
                socialY = centerY - optionSize / 2 - (Math.floor((nLeft - 1) / 2) + 1) * (socialSize + socialMargin);
            } else {
                nRight += 1;
                if (nRight % 2 === 1) {
                    rightColumn = -1;
                } else {
                    rightColumn = 1;
                }
                personX = centerX + optionDodge - socialSize / 2 + socialDodge * rightColumn;
                socialY = centerY - optionSize / 2 - (Math.floor((nRight - 1) / 2) + 1) * (socialSize + socialMargin);
            }
            ctx.strokeStyle = "rgba(255,193,7," + alpha + ")";
            ctx.lineWidth = defaultLineWidth * 4;
            ctx.drawImage(image, personX, socialY, socialSize, socialSize);

            // Record social RT
            if (response.rt_other.length !== response.choice_other.length) {
                socialInfoTime = performance.now();
                response.rt_other.push(Math.round(socialInfoTime - startTimeTrial));
                response.frame_other.push(frameCounter + 1); // Number of samples, not index
            }
        }
    }
}

function drawTimer(remainingTime) {
    ctx.fillStyle = "#dc3545";
    ctx.font = `${textSize * 1.5}pt Roboto`;
    ctx.fillText(String(Math.ceil(remainingTime / 1000)), centerX, centerY + optionSize);
}

////////////////////////
// Animation function //
////////////////////////

let requestID;
let alpha = 1;
let loopCounter = 0;
let frameCounter = 0;

function drawAnimation() {

    // Animation
    ctx.clearRect(0, 0, canvasWidth, canvasHeight);
    requestID = requestAnimationFrame(drawAnimation);
    drawOption(pressedKey);
    
    // Start the trial
    if (loopCounter === 0) {
        body.addEventListener("keydown", keyPressEvent);
        body.addEventListener("click", mouseClickEvent);
        body.addEventListener("mousemove", mouseMoveEvent);
        startTimeTrial = startTimeSample = performance.now();
    }
    
    // Restart each sample
    if (feedbackTime >= feedbackDuration) {
        // Reset flugs
        isSampledSelf = false;
        if (condition === "social") {
            if (nSampledPlayers[frameCounter] + nChosenPlayers[frameCounter] !== nPlayers) {
                nPlayers = nSampledPlayers[frameCounter] + nChosenPlayers[frameCounter];
            }
            if (nPlayers === 1) {
                response.is_dropout_other = true;
            }
            nSampledPlayers[frameCounter + 1] = 0;
            nChosenPlayers[frameCounter + 1] = nChosenPlayers[frameCounter];
            isSampledPlayers = false;
            if (nChosenPlayers[frameCounter] !== nPlayers) {
                isChosenPlayers = false;
            }
        }
        frameCounter += 1;
        if (isSampledLeft && isSampledRight) {
            isSampledBoth = checkSampledBoth(isSampledLeft, isSampledRight);
        } else if (frameCounter == maxSample) {
            isSampledBoth = true;
        }
        isFeedbackStarted = false;
        feedbackTime = 0;
        startTimeFeedback = 0;
        isTimeout = false;
        isTimer = false;
        alpha = 1;
        
        if (!isChosenSelf) {
            response.is_dropout_self = true;
        }
        startTimeSample = performance.now();
        isLateOthers = false;
    }

    // Record time
    currentTime = performance.now();
    elapsedTimeTrial = currentTime - startTimeTrial;
    elapsedTimeSample = currentTime - startTimeSample;

    // Timer
    remainingTime = timeLimitSample - elapsedTimeSample;
    if ((!isSampledSelf && !isChosenSelf) && remainingTime < alertDuration && remainingTime > 0) {
        drawTimer(remainingTime);
        if (!isTimer) {
            isTimer = true;
        }
    }

    // Message flug (= after the timeout)
    if (elapsedTimeSample > sampleDuration) {
        isTimeout = true;
    }

    // Message flug (when the partners are late)
    timeLag = currentTime - startTimeSample;
    if (timeLag >= 5000 && (isSampledSelf || isChosenSelf) && (!isSampledPlayers && !isChosenPlayers)) {
        isLateOthers = true;
    }

    // If no sampling or choice
    if (!isFeedbackStarted && elapsedTimeSample > timeLimitSample) {
        // Update the number of players
        nPlayers = nSampledPlayers[frameCounter] + nChosenPlayers[frameCounter];
        if (nChosenPlayers[frameCounter] === nPlayers) {
            isChosenPlayers = true;
        } else if (nSampledPlayers[frameCounter] + nChosenPlayers[frameCounter] === nPlayers) {
            isSampledPlayers = true;
        }
        if (!isSampledSelf && !isChosenSelf) {
            endTrial();
        }
    }

    // Feedback and proceed to the next sample
    if (isFeedbackStarted) {
        // Change the transparency
        if (currentTime - startTimeFeedback > alphaDelay) {
            alpha = 1 - (currentTime - startTimeFeedback - alphaDelay) / alphaDuration;
        } else {
            alpha = 1;
        }
        feedbackTime = performance.now() - startTimeFeedback;
    }

    // End the trial
    endAllPlayers();
    loopCounter += 1;
}

//////////////////////////////
// Initialize the variables //
//////////////////////////////

let isSampledSelf = false;
let isChosenSelf = false;
let isSampledPlayers = false;
let pressedKey;
let clickedKey;
let startTimeTrial;
let isSocialRecorded = false;
let isTimeout = false;
let startTimeFeedback = 0;
let feedbackTime = 0;
let isFeedbackStarted = false;
let isHoverLeft = false;
let isHoverRight = false;
let isSocialShown = false;
let isSampledLeft = false;
let isSampledRight = false;
let isSampledBoth = false;
let isTimer = false;
let isRoundCompleted = false;
let isLateOthers = false;
let responseTime;
let nSampledPlayers = Array(maxSample + 1).fill(0);
let nChosenPlayers = Array(maxSample + 1).fill(0);
let socialInfoPosition = [];

// Run the trial
let isChosenPlayers;
if (condition === "social") {
    isSampledPlayers = false;
    isChosenPlayers = false;
} else {
    isSampledPlayers = true;
    isChosenPlayers = true;
}

window.addEventListener("beforeunload", function (event) {
    if (!isRoundCompleted) {
        event.preventDefault();
    }
});

drawRoundNumber(roundNumber, totalRoundNumber);
if (condition !== "practice") {
    liveSend("started");
}
setTimeout(drawAnimation, iti);
