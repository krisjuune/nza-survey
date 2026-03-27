Qualtrics.SurveyEngine.addOnReady(function () {

    console.log("=== FRAMING TEXT JS START ===");

    var taskNumber = parseInt("${lm://CurrentLoopNumber}");
    var lang = "${e://Field/Q_Language}";

    console.log("Language:", lang);
    console.log("Task number:", taskNumber);

    // ------------------------------------
    // 1. PRE-PIPE JS VARIABLES
    // ------------------------------------
    function pick(arr, idx) {
        return arr[idx - 1];
    }

    var a_fuel = pick([
        "${e://Field/__js_task1_fuel1_code}",
        "${e://Field/__js_task2_fuel1_code}",
        "${e://Field/__js_task3_fuel1_code}",
        "${e://Field/__js_task4_fuel1_code}",
        "${e://Field/__js_task5_fuel1_code}",
        "${e://Field/__js_task6_fuel1_code}"
    ], taskNumber);

    var b_fuel = pick([
        "${e://Field/__js_task1_fuel2_code}",
        "${e://Field/__js_task2_fuel2_code}",
        "${e://Field/__js_task3_fuel2_code}",
        "${e://Field/__js_task4_fuel2_code}",
        "${e://Field/__js_task5_fuel2_code}",
        "${e://Field/__js_task6_fuel2_code}"
    ], taskNumber);

    var a_activity = pick([
        "${e://Field/__js_task1_activity1_code}",
        "${e://Field/__js_task2_activity1_code}",
        "${e://Field/__js_task3_activity1_code}",
        "${e://Field/__js_task4_activity1_code}",
        "${e://Field/__js_task5_activity1_code}",
        "${e://Field/__js_task6_activity1_code}"
    ], taskNumber);

    var b_activity = pick([
        "${e://Field/__js_task1_activity2_code}",
        "${e://Field/__js_task2_activity2_code}",
        "${e://Field/__js_task3_activity2_code}",
        "${e://Field/__js_task4_activity2_code}",
        "${e://Field/__js_task5_activity2_code}",
        "${e://Field/__js_task6_activity2_code}"
    ], taskNumber);

    var a_durability = pick([
        "${e://Field/__js_task1_durability1_code}",
        "${e://Field/__js_task2_durability1_code}",
        "${e://Field/__js_task3_durability1_code}",
        "${e://Field/__js_task4_durability1_code}",
        "${e://Field/__js_task5_durability1_code}",
        "${e://Field/__js_task6_durability1_code}"
    ], taskNumber);

    var b_durability = pick([
        "${e://Field/__js_task1_durability2_code}",
        "${e://Field/__js_task2_durability2_code}",
        "${e://Field/__js_task3_durability2_code}",
        "${e://Field/__js_task4_durability2_code}",
        "${e://Field/__js_task5_durability2_code}",
        "${e://Field/__js_task6_durability2_code}"
    ], taskNumber);

    console.log("A codes:", a_fuel, a_activity, a_durability);
    console.log("B codes:", b_fuel, b_activity, b_durability);

    // ------------------------------------
    // 2. NET ZERO CLASSIFICATION
    // ------------------------------------
    function isNetZeroAligned(fuel, activity, durability) {
        return (
            fuel === "plants" ||
            fuel === "electric" ||
            (
                (activity === "trees" || activity === "direct_air") &&
                durability === "permanent"
            )
        );
    }

    var a_nz = isNetZeroAligned(a_fuel, a_activity, a_durability);
    var b_nz = isNetZeroAligned(b_fuel, b_activity, b_durability);

    console.log("A NZ:", a_nz);
    console.log("B NZ:", b_nz);

    Qualtrics.SurveyEngine.setJSEmbeddedData("a_nz_binary_task" + taskNumber, a_nz ? 1 : 0);
    Qualtrics.SurveyEngine.setJSEmbeddedData("b_nz_binary_task" + taskNumber, b_nz ? 1 : 0);

    // ------------------------------------
    // 3. TRANSLATIONS (EXPLICIT)
    // ------------------------------------
    var texts;

    switch (lang) {

        case "DE":
            texts = {
                a_nz: "Paket A: Die Luftfahrt trägt nicht länger zur globalen Erwärmung bei.",
                a_not_nz: "Paket A: Die Luftfahrt trägt weiterhin zur globalen Erwärmung bei.",
                b_nz: "Paket B: Die Luftfahrt trägt nicht länger zur globalen Erwärmung bei.",
                b_not_nz: "Paket B: Die Luftfahrt trägt weiterhin zur globalen Erwärmung bei."
            };
            break;

        case "AR":
            texts = {
                a_nz: "المجموعة أ: الطيران لا يساهم في استمرار الاحتباس العالمي.",
                a_not_nz: "المجموعة أ: الطيران يساهم في استمرار الاحتباس العالمي.",
                b_nz: "المجموعة ب: الطيران لا يساهم في استمرار الاحتباس العالمي.",
                b_not_nz: "المجموعة ب: الطيران يساهم في استمرار الاحتباس العالمي."
            };
            break;

        case "VI":
            texts = {
                a_nz: "Gói A: hàng không không tiếp tục góp phần vào tình trạng nóng lên toàn cầu.",
                a_not_nz: "Gói A: hàng không tiếp tục góp phần vào tình trạng nóng lên toàn cầu.",
                b_nz: "Gói B: hàng không không tiếp tục góp phần vào tình trạng nóng lên toàn cầu.",
                b_not_nz: "Gói B: hàng không tiếp tục góp phần vào tình trạng nóng lên toàn cầu."
            };
            break;

        case "PT-BR":
            texts = {
                a_nz: "Pacote A: a aviação não continua contribuindo para o aquecimento global.",
                a_not_nz: "Pacote A: A aviação continua contribuindo para o aquecimento global.",
                b_nz: "Pacote B: a aviação não continua contribuindo para o aquecimento global.",
                b_not_nz: "Pacote B: A aviação continua contribuindo para o aquecimento global."
            };
            break;

        default: // EN
            texts = {
                a_nz: "Package A: aviation does not continue to contribute to global warming.",
                a_not_nz: "Package A: aviation continues to contribute to global warming.",
                b_nz: "Package B: aviation does not continue to contribute to global warming.",
                b_not_nz: "Package B: aviation continues to contribute to global warming."
            };
    }

    // ------------------------------------
    // 4. TEXT SELECTION
    // ------------------------------------
    var a_text = a_nz ? texts.a_nz : texts.a_not_nz;
    var b_text = b_nz ? texts.b_nz : texts.b_not_nz;

    console.log("Final A text:", a_text);
    console.log("Final B text:", b_text);

    // ------------------------------------
    // 5. RENDER
    // ------------------------------------
    var container = this.getQuestionTextContainer();

    if (!container) {
        console.error("No container found");
        return;
    }

    if (lang === "AR") {
        container.setAttribute("dir", "rtl");
        container.style.textAlign = "right";
    }

    container.innerHTML =
        "<p>" + a_text + "</p>" +
        "<p>" + b_text + "</p>";

    console.log("=== FRAMING TEXT JS END ===");
});
