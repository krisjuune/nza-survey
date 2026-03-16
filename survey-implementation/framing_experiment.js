Qualtrics.SurveyEngine.addOnReady(function () {

    console.log("=== FRAMING TEXT JS START ===");

    function normalizeFuel(fuelText) {
        console.log("Raw fuel:", fuelText);
        if (fuelText === "Fuels made using clean electricity") return "E-SAF";
        if (fuelText === "Bio-based fuels") return "Bio-SAF";
        return "Other";
    }

    function normalizeActivity(activityText) {
        console.log("Raw activity:", activityText);
        if (activityText === "Capturing CO2 from the air and storing it in rocks deep underground") return "DACCS";
        if (activityText === "Planting and maintaining forests") return "Trees";
        return "Other";
    }

    function normalizeDurability(durText) {
        console.log("Raw durability:", durText);
        if (durText === "Permanent") return "Perm";
        if (durText === "Temporary") return "Temp";
        return "Other";
    }

    function isNetZeroAligned(fuel, activity, durability) {
        return (
            fuel === "Bio-SAF" ||
            fuel === "E-SAF" ||
            ((activity === "Trees" || activity === "DACCS") && durability === "Perm")
        );
    }

    var a_fuel_raw = "${lm://Field/1}";
    var a_activity_raw = "${lm://Field/3}";
    var a_durability_raw = "${lm://Field/5}";

    var b_fuel_raw = "${lm://Field/2}";
    var b_activity_raw = "${lm://Field/4}";
    var b_durability_raw = "${lm://Field/6}";

    console.log("A raw:", a_fuel_raw, a_activity_raw, a_durability_raw);
    console.log("B raw:", b_fuel_raw, b_activity_raw, b_durability_raw);

    var a_fuel = normalizeFuel(a_fuel_raw);
    var a_activity = normalizeActivity(a_activity_raw);
    var a_durability = normalizeDurability(a_durability_raw);

    var b_fuel = normalizeFuel(b_fuel_raw);
    var b_activity = normalizeActivity(b_activity_raw);
    var b_durability = normalizeDurability(b_durability_raw);

    console.log("A normalized:", a_fuel, a_activity, a_durability);
    console.log("B normalized:", b_fuel, b_activity, b_durability);

    var a_nz = isNetZeroAligned(a_fuel, a_activity, a_durability);
    var b_nz = isNetZeroAligned(b_fuel, b_activity, b_durability);
    var taskNumber = "${lm://CurrentLoopNumber}";

    Qualtrics.SurveyEngine.setJSEmbeddedData("a_nz_binary_task" + taskNumber, a_nz ? 1 : 0);
    Qualtrics.SurveyEngine.setJSEmbeddedData("b_nz_binary_task" + taskNumber, b_nz ? 1 : 0);

    console.log("A NZ:", a_nz);
    console.log("B NZ:", b_nz);

    var a_text_nz     = '${e://Field/a_text_nz}';
	var a_text_not_nz = '${e://Field/a_text_not_nz}';
	var b_text_nz     = '${e://Field/b_text_nz}';
	var b_text_not_nz = '${e://Field/b_text_not_nz}';


    console.log("SF texts:",
        a_text_nz,
        a_text_not_nz,
        b_text_nz,
        b_text_not_nz
    );

    var a_text = a_nz ? a_text_nz : a_text_not_nz;
    var b_text = b_nz ? b_text_nz : b_text_not_nz;

    console.log("Final A text:", a_text);
    console.log("Final B text:", b_text);

    var container = this.getQuestionTextContainer();
    console.log("Text container:", container);

    if (!container) {
        console.error("Question text container not found");
        return;
    }

    container.innerHTML =
        "<p>" + a_text + "</p>" +
        "<p>" + b_text + "</p>";

    console.log("=== FRAMING TEXT JS END ===");
});