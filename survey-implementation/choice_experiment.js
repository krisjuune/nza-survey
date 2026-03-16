Qualtrics.SurveyEngine.addOnload(function () {
    this.hidePreviousButton();

    var numChoice = 6;

    var optionName;
    var taskName;

    var fuelName;
    var fuelDescription;

    var activityName;
    var activityDescription;

    var durabilityName;
    var durabilityDescription;

    var responsibilityName;
    var responsibilityDescription;

    var costName;
    var costDescription;

    var fuelDict;
    var activityDict;
    var durabilityDict;
    var responsibilityDict;
    var costDict;


    switch ("${e://Field/Q_Language}") {

        // -----------------------------
        // GERMAN
        // -----------------------------
        case "DE":

            optionName = "Paket";
            taskName = "Aufgabe";

            fuelName = "Treibstoffart";
            fuelDescription = "Flugzeugtreibstoffe können herkömmliche fossile Treibstoffe oder Alternativen sein, die insgesamt weniger CO2-Emissionen verursachen.";

            activityName = "Aktivitätstyp";
            activityDescription = "Die Luftfahrt kann verschiedene Aktivitäten finanzieren, um entweder CO2-Emissionen in anderen Sektoren zu reduzieren oder CO2 aus der Atmosphäre zu entfernen.";

            durabilityName = "Langlebigkeit";
            durabilityDescription = "CO2 kann dauerhaft oder vorübergehend gespeichert werden. Vorübergehende Speicherung bedeutet, dass CO2 später wieder in die Atmosphäre freigesetzt werden kann.";

            responsibilityName = "Verantwortung";
            responsibilityDescription = "Verschiedene Gruppen könnten dafür verantwortlich sein, die durch das Fliegen verursachten CO2-Emissionen auszugleichen.";

            costName = "Kostenanstieg";
            costDescription = "Dies sind die zusätzlichen Kosten für Ihren Flug, dargestellt als prozentuale Erhöhung gegenüber einem Flugticket heute.";


            fuelDict = {
                "fossil": "Herkömmlicher fossiler Treibstoff",
                "plants": "Aus Pflanzen hergestellter Treibstoff",
                "electric": "Aus sauberem Strom hergestellter Treibstoff"
            };

            activityDict = {
                "trees": "Pflanzen neuer Bäume",
                "cookstoves": "Austausch von Kohleherden durch Elektroherde",
                "factory_ccs": "Auffangen von CO2 in Fabriken und unterirdische Speicherung",
                "direct_air": "Auffangen von CO2 aus der Atmosphäre und unterirdische Speicherung"
            };

            durabilityDict = {
                "permanent": "Dauerhaft",
                "temporary": "Vorübergehend"
            };

            responsibilityDict = {
                "airline": "Fluggesellschaften",
                "fuel_suppliers": "Treibstofflieferanten",
                "passenger": "Passagiere",
                "government": "Regierungen"
            };

            costDict = {
                "10": "+10 %",
                "30": "+30 %",
                "50": "+50 %"
            };

        break;

        // -----------------------------
        // BRAZILIAN PORTUGUESE
        // -----------------------------
        case "PT-BR":

            optionName = "Pacote";
            taskName = "Tarefa";

            fuelName = "Tipo de combustível";
            fuelDescription = "Os combustíveis de avião podem ser combustíveis fósseis comuns ou alternativas que causam menos emissões totais de CO2.";

            activityName = "Tipo de atividade";
            activityDescription = "A aviação pode financiar diferentes atividades para reduzir as emissões de CO2 em outros setores ou remover CO2 da atmosfera.";

            durabilityName = "Durabilidade";
            durabilityDescription = "O CO2 pode ser armazenado permanentemente ou temporariamente. O armazenamento temporário significa que o CO2 pode ser liberado de volta para a atmosfera em algum momento no futuro.";

            responsibilityName = "Responsabilidade";
            responsibilityDescription = "Diferentes grupos podem ser responsáveis por compensar o CO2 causado pelos voos.";

            costName = "Aumento de custo";
            costDescription = "Este é o custo adicional para o seu voo, apresentado como um aumento percentual em comparação com o preço de uma passagem hoje.";


            fuelDict = {
                "fossil": "Combustíveis fósseis comuns",
                "plants": "Combustíveis feitos de plantas",
                "electric": "Combustíveis produzidos usando eletricidade limpa"
            };

            activityDict = {
                "trees": "Plantar novas árvores",
                "cookstoves": "Substituir fogões a lenha por elétricos",
                "factory_ccs": "Capturar o CO2 das fábricas e armazená-lo em rochas no subsolo",
                "direct_air": "Capturar CO2 do ar e armazená-lo em rochas no subsolo"
            };

            durabilityDict = {
                "permanent": "Permanente",
                "temporary": "Temporária"
            };

            responsibilityDict = {
                "airline": "Companhia aérea",
                "fuel_suppliers": "Fornecedores de combustível",
                "passenger": "Passageiro",
                "government": "Governo"
            };

            costDict = {
                "10": "+10%",
                "30": "+30%",
                "50": "+50%"
            };

        break;



        // -----------------------------
        // ARABIC (UAE)
        // -----------------------------
        case "AR":

            optionName = "حزمة";
            taskName = "مهمة";

            fuelName = "نوع الوقود";
            fuelDescription = "يمكن أن تكون وقود الطائرات وقودًا أحفوريًا عاديًا أو بدائل تسبب انبعاثات أقل من ثاني أكسيد الكربون بشكل عام.";

            activityName = "نوع النشاط";
            activityDescription = "يمكن لقطاع الطيران تمويل أنشطة مختلفة إما لتقليل انبعاثات ثاني أكسيد الكربون في قطاعات أخرى أو لإزالة ثاني أكسيد الكربون من الغلاف الجوي.";

            durabilityName = "المتانة";
            durabilityDescription = "يمكن تخزين ثاني أكسيد الكربون بشكل دائم أو مؤقت. التخزين المؤقت يعني أنه قد يتم إطلاق ثاني أكسيد الكربون مرة أخرى في الغلاف الجوي في وقت لاحق.";

            responsibilityName = "المسؤولية";
            responsibilityDescription = "يمكن أن تكون مجموعات مختلفة مسؤولة عن موازنة ثاني أكسيد الكربون الناتج عن الطيران.";

            costName = "زيادة التكلفة";
            costDescription = "هذه هي التكلفة الإضافية لرحلتك، معروضة كنسبة زيادة مقارنة بسعر تذكرة الطيران اليوم.";


            fuelDict = {
                "fossil": "الوقود الأحفوري العادي",
                "plants": "الوقود المصنوع من النباتات",
                "electric": "الوقود المصنوع باستخدام كهرباء نظيفة"
            };

            activityDict = {
                "trees": "زراعة أشجار جديدة",
                "cookstoves": "استبدال مواقد الطهي بأخرى كهربائية",
                "factory_ccs": "احتجاز ثاني أكسيد الكربون من المصانع وتخزينه في صخور عميقة تحت الأرض",
                "direct_air": "احتجاز ثاني أكسيد الكربون من الهواء وتخزينه في صخور عميقة تحت الأرض"
            };

            durabilityDict = {
                "permanent": "دائم",
                "temporary": "مؤقت"
            };

            responsibilityDict = {
                "airline": "شركة طيران",
                "fuel_suppliers": "مورّدو الوقود",
                "passenger": "راكب/مسافر",
                "government": "جهة حكومية"
            };

            costDict = {
                "10": "أكثر من 10%",
                "30": "أكثر من 30%",
                "50": "أكثر من 50%"
            };

        break;



        // -----------------------------
        // VIETNAMESE
        // -----------------------------
        case "VI":

            optionName = "Gói";
            taskName = "Nhiệm vụ";

            fuelName = "Loại nhiên liệu";
            fuelDescription = "Nhiên liệu máy bay có thể là nhiên liệu hóa thạch thông thường hoặc các lựa chọn thay thế gây ra ít phát thải CO2 hơn.";

            activityName = "Loại hoạt động";
            activityDescription = "Ngành hàng không có thể tài trợ cho các hoạt động khác nhau để giảm phát thải CO2 trong các lĩnh vực khác hoặc loại bỏ CO2 khỏi khí quyển.";

            durabilityName = "Độ bền";
            durabilityDescription = "CO2 có thể được lưu trữ lâu dài hoặc tạm thời. Lưu trữ tạm thời có nghĩa là CO2 có thể được giải phóng trở lại khí quyển vào một thời điểm sau đó.";

            responsibilityName = "Trách nhiệm";
            responsibilityDescription = "Các nhóm khác nhau có thể chịu trách nhiệm bù đắp lượng CO2 do việc bay gây ra.";

            costName = "Tăng chi phí";
            costDescription = "Đây là chi phí bổ sung cho chuyến bay của bạn, được hiển thị dưới dạng phần trăm tăng so với giá vé máy bay hiện nay.";


            fuelDict = {
                "fossil": "Nhiên liệu hóa thạch thông thường",
                "plants": "Nhiên liệu làm từ thực vật",
                "electric": "Nhiên liệu làm bằng điện sạch"
            };

            activityDict = {
                "trees": "Trồng cây mới",
                "cookstoves": "Thay bếp củi bằng bếp điện",
                "factory_ccs": "Thu giữ CO2 từ các nhà máy và lưu trữ trong đá sâu dưới lòng đất",
                "direct_air": "Thu giữ CO2 từ không khí và lưu trữ trong đá sâu dưới lòng đất"
            };

            durabilityDict = {
                "permanent": "Lâu dài",
                "temporary": "Tạm thời"
            };

            responsibilityDict = {
                "airline": "Hàng không",
                "fuel_suppliers": "Nhà cung cấp nhiên liệu",
                "passenger": "Hành khách",
                "government": "Chính phủ"
            };

            costDict = {
                "10": "+10%",
                "30": "+30%",
                "50": "+50%"
            };

        break;




        // -----------------------------
        // DEFAULT (ENGLISH)
        // -----------------------------
        default:

            optionName = "Package";
            taskName = "Task";

            fuelName = "Fuel type";
            fuelDescription = "Airplane fuels can be regular fossil fuels, or alternatives that cause less CO2 emissions overall.";

            activityName = "Activity type";
            activityDescription = "Aviation can fund different activities to either reduce CO2 emissions in other sectors or remove CO2 from the atmosphere.";

            durabilityName = "Durability";
            durabilityDescription = "CO2 can be stored away permanently or temporarily. Temporary storage means that the CO2 may be released back into the atmosphere later.";

            responsibilityName = "Responsibility";
            responsibilityDescription = "Different groups could be responsible for balancing out the CO2 caused by flying.";

            costName = "Cost increase";
            costDescription = "This is the extra cost to your flight, shown as a percentage increase compared to a flight ticket today.";


            fuelDict = {
                "fossil": "Regular fossil fuels",
                "plants": "Fuels made from plants",
                "electric": "Fuels made using clean electricity"
            };

            activityDict = {
                "trees": "Planting new trees",
                "cookstoves": "Replacing cookstoves with electric ones",
                "factory_ccs": "Capturing CO2 from factories and storing it underground",
                "direct_air": "Capturing CO2 from the air and storing it underground"
            };

            durabilityDict = {
                "permanent": "Permanent",
                "temporary": "Temporary"
            };

            responsibilityDict = {
                "airline": "Airline",
                "fuel_suppliers": "Fuel suppliers",
                "passenger": "Passenger",
                "government": "Government"
            };

            costDict = {
                "10": "+10%",
                "30": "+30%",
                "50": "+50%"
            };

    }



    // Store attribute names/descriptions
    Qualtrics.SurveyEngine.setJSEmbeddedData("option_name", optionName);
    Qualtrics.SurveyEngine.setJSEmbeddedData("task_name", taskName);

    Qualtrics.SurveyEngine.setJSEmbeddedData("fuel_name", fuelName);
    Qualtrics.SurveyEngine.setJSEmbeddedData("fuel_description", fuelDescription);

    Qualtrics.SurveyEngine.setJSEmbeddedData("activity_name", activityName);
    Qualtrics.SurveyEngine.setJSEmbeddedData("activity_description", activityDescription);

    Qualtrics.SurveyEngine.setJSEmbeddedData("durability_name", durabilityName);
    Qualtrics.SurveyEngine.setJSEmbeddedData("durability_description", durabilityDescription);

    Qualtrics.SurveyEngine.setJSEmbeddedData("responsibility_name", responsibilityName);
    Qualtrics.SurveyEngine.setJSEmbeddedData("responsibility_description", responsibilityDescription);

    Qualtrics.SurveyEngine.setJSEmbeddedData("cost_name", costName);
    Qualtrics.SurveyEngine.setJSEmbeddedData("cost_description", costDescription);



    function shuffle(array) {
        for (var i = array.length - 1; i > 0; i--) {
            var j = Math.floor(Math.random() * (i + 1));
            var temp = array[i];
            array[i] = array[j];
            array[j] = temp;
        }
        return array;
    }

    function shuffle_one(theArray) {
        return shuffle(theArray)[0];
    }


    for (var i = 1; i <= numChoice; i++) {

        fuel1 = shuffle_one(Object.keys(fuelDict));
        fuel2 = shuffle_one(Object.keys(fuelDict));

        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_fuel1_code", fuel1);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_fuel2_code", fuel2);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_fuel1", fuelDict[fuel1]);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_fuel2", fuelDict[fuel2]);

        activity1 = shuffle_one(Object.keys(activityDict));
        activity2 = shuffle_one(Object.keys(activityDict));

        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_activity1_code", activity1);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_activity2_code", activity2);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_activity1", activityDict[activity1]);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_activity2", activityDict[activity2]);

        durability1 = shuffle_one(Object.keys(durabilityDict));
        durability2 = shuffle_one(Object.keys(durabilityDict));

        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_durability1_code", durability1);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_durability2_code", durability2);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_durability1", durabilityDict[durability1]);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_durability2", durabilityDict[durability2]);

        responsibility1 = shuffle_one(Object.keys(responsibilityDict));
        responsibility2 = shuffle_one(Object.keys(responsibilityDict));

        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_responsibility1_code", responsibility1);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_responsibility2_code", responsibility2);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_responsibility1", responsibilityDict[responsibility1]);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_responsibility2", responsibilityDict[responsibility2]);

        cost1 = shuffle_one(Object.keys(costDict));
        cost2 = shuffle_one(Object.keys(costDict));

        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_cost1_code", cost1);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_cost2_code", cost2);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_cost1", costDict[cost1]);
        Qualtrics.SurveyEngine.setJSEmbeddedData("task" + i + "_cost2", costDict[cost2]);
    }

});