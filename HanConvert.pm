package Lingua::ZH::HanConvert;

use strict;
use utf8;

BEGIN {
    our ($VERSION, @ISA, @EXPORT_OK);
    use Exporter;
    @ISA = qw(Exporter);
    @EXPORT_OK = qw(&trad &simple);
    $VERSION = "0.01";
}

my @table = qw(
  T:萬與醜專業叢東絲兩嚴喪個豐臨爲麗舉麽義烏樂喬習鄉
  S:万与丑专业丛东丝两严丧个丰临为丽举么义乌乐乔习乡
  T:書買亂爭虧雲亞産畝親褻億僅僕從侖倉儀們價衆優會傴
  S:书买乱争亏云亚产亩亲亵亿仅仆从仑仓仪们价众优会伛
  T:傘偉傳傷倀倫傖僞佇體餘傭僉俠僥偵側僑儈儕儂儔儼倆
  S:伞伟传伤伥伦伧伪伫体余佣佥侠侥侦侧侨侩侪侬俦俨俩
  T:儷儉債傾僂僨償儻儐儲儺兒黨蘭關興養獸囅岡寫軍農馮
  S:俪俭债倾偻偾偿傥傧储傩儿党兰关兴养兽冁冈写军农冯
  T:衝凍凈準幾鳳鳬憑凱擊鑿芻劃劉則剛創剄劊劌剴劑剮劍
  S:冲冻净准几凤凫凭凯击凿刍划刘则刚创刭刽刿剀剂剐剑
  T:劇勸辦務勱動勵勁勞勢勛匭匱區醫華協單賣盧滷衛廠廳
  S:剧劝办务劢动励劲劳势勋匦匮区医华协单卖卢卤卫厂厅
  T:歷厲壓厭厙厠厴縣參雙發髮變疊衹臺葉號嘆嘰後嚇嗎噸
  S:历厉压厌厍厕厣县参双发发变叠只台叶号叹叽后吓吗吨
  T:聽啓嘸囈嘔嚦唄員咼嗆嗚嚨嚀噝響啞噠嘵嗶噦嘩噲嚌噥
  S:听启呒呓呕呖呗员呙呛呜咙咛咝响哑哒哓哔哕哗哙哜哝
  T:喲嘜嘮嗩喚嘖嗇囀嚙嘯噴嘍嚳囁噯嚶囑嚕囂團園圍圇國
  S:哟唛唠唢唤啧啬啭啮啸喷喽喾嗫嗳嘤嘱噜嚣团园围囵国
  T:圖圓聖壙場壞塊堅壇壢壩塢墳墜壟壠壚壘墾堊墊埡塏堖
  S:图圆圣圹场坏块坚坛坜坝坞坟坠垄垅垆垒垦垩垫垭垲垴
  T:塒塤堝塹墮墻壯聲殻壺處備復頭誇夾奪奩奐奮奬妝婦媽
  S:埘埙埚堑堕墙壮声壳壶处备复头夸夹夺奁奂奋奖妆妇妈
  T:嫵嫗嬀婁婭嬈嬌孌媧嫻嬰嬋嬸嬡嬪嬙孫學孿寧寶實寵審
  S:妩妪妫娄娅娆娇娈娲娴婴婵婶嫒嫔嫱孙学孪宁宝实宠审
  T:憲寬賓寢對尋導壽將爾塵嘗堯尷盡層屬屢屨嶼歲豈嶇崗
  S:宪宽宾寝对寻导寿将尔尘尝尧尴尽层属屡屦屿岁岂岖岗
  T:峴嵐島嶺崬巋嶧峽嶠崢巒嶗崍嶄嶸嶁巔鞏巰幣帥師幃帳
  S:岘岚岛岭岽岿峄峡峤峥峦崂崃崭嵘嵝巅巩巯币帅师帏帐
  T:簾幟帶幀幫幬幘幗幹廣莊慶廬廡庫應廟龐廢開棄張彌弳
  S:帘帜带帧帮帱帻帼干广庄庆庐庑库应庙庞废开弃张弥弪
  T:彎彈歸當録徹徑后徠憶懺憂愾懷態慫憮慪悵愴憐總懟懌
  S:弯弹归当录彻径後徕忆忏忧忾怀态怂怃怄怅怆怜总怼怿
  T:戀懇惡慟懨愷惻惱惲慤懸慳憫驚懼慘懲憊愜慚憚慣憤憒
  S:恋恳恶恸恹恺恻恼恽悫悬悭悯惊惧惨惩惫惬惭惮惯愤愦
  T:願懾懣懶戇戔戲戧戰戩撲執擴捫掃揚擾撫摶摳掄搶護報
  S:愿慑懑懒戆戋戏戗战戬扑执扩扪扫扬扰抚抟抠抡抢护报
  T:擔擬攏揀擁攔擰撥擇摯攣撾撻挾撓擋撟掙擠揮撈損撿換
  S:担拟拢拣拥拦拧拨择挚挛挝挞挟挠挡挢挣挤挥捞损捡换
  T:搗據擄摑擲撣摻摜攬撳攙擱摟攪攝攄擺擯攤攖攆擷擼攛
  S:捣据掳掴掷掸掺掼揽揿搀搁搂搅摄摅摆摈摊撄撵撷撸撺
  T:擻攢敵斂數齋斕鬥斬斷無舊時曠曇晝顯晉曬曉曄暈暉暫
  S:擞攒敌敛数斋斓斗斩断无旧时旷昙昼显晋晒晓晔晕晖暂
  T:曖術樸機殺雜權條來楊榪鬆極構樅樞棗櫪梘棖槍楓梟櫃
  S:暧术朴机杀杂权条来杨杩松极构枞枢枣枥枧枨枪枫枭柜
  T:檸檉標棧櫛櫳棟櫨櫟欄樹棲樣欒椏橈楨檔榿橋樺檜槳樁
  S:柠柽标栈栉栊栋栌栎栏树栖样栾桠桡桢档桤桥桦桧桨桩
  T:夢檢欞櫝槧欏橢樓欖櫬櫚櫸檻檳櫧檣櫻櫓櫞歡歟歐殲殤
  S:梦检棂椟椠椤椭楼榄榇榈榉槛槟槠樯樱橹橼欢欤欧歼殇
  T:殘殞殮殫殯毆轂畢斃氈毿氌氣氫氬匯漢湯溝灃漚瀝淪滄
  S:残殒殓殚殡殴毂毕毙毡毵氇气氢氩汇汉汤沟沣沤沥沦沧
  T:潙滬濘澩瀧瀘濼瀉潑澤涇潔灑窪浹淺漿澆湞濁測澮濟瀏
  S:沩沪泞泶泷泸泺泻泼泽泾洁洒洼浃浅浆浇浈浊测浍济浏
  T:渾滸濃潯塗濤澇淶漣潿渦渙滌潤澗漲澀澱淵漬瀆漸澠漁
  S:浑浒浓浔涂涛涝涞涟涠涡涣涤润涧涨涩淀渊渍渎渐渑渔
  T:瀋滲灣濕潰濺潷滯灧灄滿瀅濾濫灤濱灘灕瀠瀟瀲濰潛瀾
  S:渖渗湾湿溃溅滗滞滟滠满滢滤滥滦滨滩漓潆潇潋潍潜澜
  T:瀨瀕灝滅燈靈竈燦煬爐煒熗點煉熾爍爛烴燭煩燒燁燴燙
  S:濑濒灏灭灯灵灶灿炀炉炜炝点炼炽烁烂烃烛烦烧烨烩烫
  T:燼熱煥燜燾愛爺牘牽犧犢狀獷獁猶狽獰獨狹獅獪猙獄猻
  S:烬热焕焖焘爱爷牍牵牺犊状犷犸犹狈狞独狭狮狯狰狱狲
  T:獫獵獼玀獻獺璣瑪瑋環現璽瓏琿璉瑣瓊璦瓔瓚甌電畫暢
  S:猃猎猕猡献獭玑玛玮环现玺珑珲琏琐琼瑷璎瓒瓯电画畅
  T:疇癤療瘧癘瘍癧瘡瘋癥癰痙癢瘂癆瘓癇癉瘞瘻癟癱癮癭
  S:畴疖疗疟疠疡疬疮疯症痈痉痒痖痨痪痫瘅瘗瘘瘪瘫瘾瘿
  T:癩癬癲皚皺皸盞鹽監蓋盤瞘睜睞瞼瞞矚矯磯礬礦碭碼磚
  S:癞癣癫皑皱皲盏盐监盖盘眍睁睐睑瞒瞩矫矶矾矿砀码砖
  T:硨硯碸礪礱礫礎碩硤磽確鹼礙磧磣禮禰禎禱禍禪離種積
  S:砗砚砜砺砻砾础硕硖硗确硷碍碛碜礼祢祯祷祸禅离种积
  T:稱穢穌穩穡窮竊竅竄窩窺竇窶竪競篤筆筧箋籠籩築篳篩
  S:称秽稣稳穑穷窃窍窜窝窥窦窭竖竞笃笔笕笺笼笾筑筚筛
  T:箏籌簽簡簀篋籜籮簞簫簣簍籃籬籪籟糴類糶糲糞糧糝緊
  S:筝筹签简箦箧箨箩箪箫篑篓篮篱簖籁籴类粜粝粪粮糁紧
  T:縶糹糾紆紅紂纖紇約級紈纊紀紉緯紜純紕紗綱納縱綸紛
  S:絷纟纠纡红纣纤纥约级纨纩纪纫纬纭纯纰纱纲纳纵纶纷
  T:紙紋紡紐紓綫紺紲紱練組紳細織終縐絆紼絀紹繹經紿綁
  S:纸纹纺纽纾线绀绁绂练组绅细织终绉绊绋绌绍绎经绐绑
  T:絨結絝繞絎繪給絢絳絡絶絞統綆綃絹綉綏縧繼綈績緒綾
  S:绒结绔绕绗绘给绚绛络绝绞统绠绡绢绣绥绦继绨绩绪绫
  T:續綺緋綽緔緄繩維綿綬綳綢綹綣綜綻綰緑綴緇緙緗緘緬
  S:续绮绯绰绱绲绳维绵绶绷绸绺绻综绽绾绿缀缁缂缃缄缅
  T:纜緹緲緝繢緦綞緞緶緱縋緩締縷編緡緣縉縛縟縝縫縞纏
  S:缆缇缈缉缋缌缍缎缏缑缒缓缔缕编缗缘缙缚缛缜缝缟缠
  T:縭縊縑繽縹縵縲纓縮繆繅纈繚繕繒繮繾繰繯繳纘罌網羅
  S:缡缢缣缤缥缦缧缨缩缪缫缬缭缮缯缰缱缲缳缴缵罂网罗
  T:罰罷羆羈羥翹耮耬聳聶聾職聹聯聵聰肅腸膚骯腎腫脹脅
  S:罚罢罴羁羟翘耢耧耸聂聋职聍联聩聪肃肠肤肮肾肿胀胁
  T:膽勝朧腖臚脛膠膾臟臍腦膿臠腡臉臘膩騰臏輿捨艤艦艙
  S:胆胜胧胨胪胫胶脍脏脐脑脓脔脶脸腊腻腾膑舆舍舣舰舱
  T:艫艱艷藝節薌蕪蘆蕓蓯葦藶莧萇蒼苧蘇蘋範莖蘢蔦塋煢
  S:舻艰艳艺节芗芜芦芸苁苇苈苋苌苍苎苏苹范茎茏茑茔茕
  T:繭薦莢蕘蓽蕎薈薺蕩榮葷滎犖熒蕁藎蓀蔭蕒葒葤藥萊蓮
  S:茧荐荚荛荜荞荟荠荡荣荤荥荦荧荨荩荪荫荬荭荮药莱莲
  T:蒔萵薟獲蕕瑩鶯蒓蘿螢營縈蕭薩蕆蕢蔣蔞藍薊蘺蕷鎣驀
  S:莳莴莶获莸莹莺莼萝萤营萦萧萨蒇蒉蒋蒌蓝蓟蓠蓣蓥蓦
  T:薔蘞藺藹蘄藴藪蘚虜慮蟲蟣雖蝦蠆蝕蟻螞蠶蜆蠱蠣蟶蠻
  S:蔷蔹蔺蔼蕲蕴薮藓虏虑虫虮虽虾虿蚀蚁蚂蚕蚬蛊蛎蛏蛮
  T:蟄蛺蟯螄蠐蝸蠟蠅蟈蟬螻蠑蟎釁銜補襯襖裊襪襲裝襠褳
  S:蛰蛱蛲蛳蛴蜗蜡蝇蝈蝉蝼蝾螨衅衔补衬袄袅袜袭装裆裢
  T:襝褲襇褸襤見觀規覓視覘覽覺覬覡覿覦覯覲覷觴觸觶譽
  S:裣裤裥褛褴见观规觅视觇览觉觊觋觌觎觏觐觑觞触觯誉
  T:謄訁計訂訃認譏訐訌討讓訕訖訓議訊記講諱謳詎訝訥許
  S:誊讠计订讣认讥讦讧讨让讪讫训议讯记讲讳讴讵讶讷许
  T:訛論訟諷設訪訣證詁訶評詛識詐訴診詆謅詞詘詔譯詒誆
  S:讹论讼讽设访诀证诂诃评诅识诈诉诊诋诌词诎诏译诒诓
  T:誄試詿詩詰詼誠誅詵話誕詬詮詭詢詣諍該詳詫諢詡誡誣
  S:诔试诖诗诘诙诚诛诜话诞诟诠诡询诣诤该详诧诨诩诫诬
  T:語誚誤誥誘誨誑説誦誒請諸諏諾讀諑誹課諉諛誰諗調諂
  S:语诮误诰诱诲诳说诵诶请诸诹诺读诼诽课诿谀谁谂调谄
  T:諒諄誶談誼謀諶諜謊諫諧謔謁謂諤諭諼讒諮諳諺諦謎諞
  S:谅谆谇谈谊谋谌谍谎谏谐谑谒谓谔谕谖谗谘谙谚谛谜谝
  T:謨讜謖謝謡謗謚謙謐謹謾謫謭謬譚譖譙讕譜譎讞譴譫讖
  S:谟谠谡谢谣谤谥谦谧谨谩谪谫谬谭谮谯谰谱谲谳谴谵谶
  T:貝貞負貢財責賢敗賬貨質販貪貧貶購貯貫貳賤賁貰貼貴
  S:贝贞负贡财责贤败账货质贩贪贫贬购贮贯贰贱贲贳贴贵
  T:貺貸貿費賀貽賊贄賈賄貲賃賂贜資賅贐賕賑賚賒賦賭賫
  S:贶贷贸费贺贻贼贽贾贿赀赁赂赃资赅赆赇赈赉赊赋赌赍
  T:贖賞賜賡賠賧賴贅賻賺賽賾贋贊贈贍贏贛趙趕趨趲躉躍
  S:赎赏赐赓赔赕赖赘赙赚赛赜赝赞赠赡赢赣赵赶趋趱趸跃
  T:蹌躒踐蹺蹕躚躋踴躊躓躑躡蹣躥躪躦軀車軋軌軒軔轉軛
  S:跄跞践跷跸跹跻踊踌踬踯蹑蹒蹿躏躜躯车轧轨轩轫转轭
  T:輪軟轟軲軻轤軸軹軼軤軫轢軺輕軾載輊轎輇輅較輒輔輛
  S:轮软轰轱轲轳轴轵轶轷轸轹轺轻轼载轾轿辁辂较辄辅辆
  T:輦輩輝輥輞輟輜輳輻輯輸轡轅轄輾轆轍轔辭辯辮邊遼達
  S:辇辈辉辊辋辍辎辏辐辑输辔辕辖辗辘辙辚辞辩辫边辽达
  T:遷過邁運還這進遠違連遲邇逕適選遜遞邐邏遺鄧鄺鄔郵
  S:迁过迈运还这进远违连迟迩迳适选逊递逦逻遗邓邝邬邮
  T:鄒鄴鄰鬱郟鄶鄭鄆酈鄖鄲醖醬釅釃釀釋裏鑒鑾鏨釒釓釔
  S:邹邺邻郁郏郐郑郓郦郧郸酝酱酽酾酿释里鉴銮錾钅钆钇
  T:針釘釗釙釕釷釺釧釤釩釣鍆釹釵鈣鈈鈦鉅鈍鈔鐘鈉鋇鋼
  S:针钉钊钋钌钍钎钏钐钒钓钔钕钗钙钚钛钜钝钞钟钠钡钢
  T:鈑鈐鑰欽鈞鎢鈎鈧鈁鈥鈄鈕鈀鈺錢鉦鉗鈷鉢鈳鉕鈽鈸鉞
  S:钣钤钥钦钧钨钩钪钫钬钭钮钯钰钱钲钳钴钵钶钷钸钹钺
  T:鑽鉬鉭鉀鈿鈾鐵鉑鈴鑠鉛鉚鈰鉉鉈鉍鈮鈹鐸銬銠鉺銪鋮
  S:钻钼钽钾钿铀铁铂铃铄铅铆铈铉铊铋铌铍铎铐铑铒铕铖
  T:鋏鋣鐃鐺銅鋁銱銦鎧鍘銖銑鋌銩鏵銓鎩鉿銚鉻銘錚銫鉸
  S:铗铘铙铛铜铝铞铟铠铡铢铣铤铥铧铨铩铪铫铬铭铮铯铰
  T:銥鏟銃鐋銨銀銣鑄鐒鋪錸鋱鏈鏗銷鎖鋰鋥鋤鍋鋯鋨銹銼
  S:铱铲铳铴铵银铷铸铹铺铼铽链铿销锁锂锃锄锅锆锇锈锉
  T:鋝鋒鋅鋶鐦鐧鋭銻鋃鋟鋦錒錆鍺鍩錯錨錛鍀錁錕錫錮鑼
  S:锊锋锌锍锎锏锐锑锒锓锔锕锖锗锘错锚锛锝锞锟锡锢锣
  T:錘錐錦鍁錈鍃錇錟錠鍵鋸錳錙鍥鍇鏘鍶鍔鍤鍬鍾鍛鎪鍰
  S:锤锥锦锨锩锪锫锬锭键锯锰锱锲锴锵锶锷锸锹锺锻锼锾
  T:鎄鍍鎂鏤鐨鎇鏌鎮鎘鑷鎸鎳鎿鎦鎬鎊鎰鎵鑌鏢鏜鏝鏍鏞
  S:锿镀镁镂镄镅镆镇镉镊镌镍镎镏镐镑镒镓镔镖镗镘镙镛
  T:鏡鏑鏃鏇鐔鐝鐐鏷鑥鐓鑭鐠鑹鏹鐙鑊鐳鐲鐮鐿鑔鑣鑲長
  S:镜镝镞镟镡镢镣镤镥镦镧镨镩镪镫镬镭镯镰镱镲镳镶长
  T:門閂閃閆閉問闖閏闈閑閎間閔閌悶閘鬧閨聞闥閩閭閥閣
  S:门闩闪闫闭问闯闰闱闲闳间闵闶闷闸闹闺闻闼闽闾阀阁
  T:閡閫鬮閲閬閾閹閶鬩閿閽閻閼闡闌闃闊闋闔闐闕闞隊陽
  S:阂阃阄阅阆阈阉阊阋阌阍阎阏阐阑阒阔阕阖阗阙阚队阳
  T:陰陣階際陸隴陳陘陝隉隕險隨隱隸難雛讎靂霧霽靄靚靜
  S:阴阵阶际陆陇陈陉陕陧陨险随隐隶难雏雠雳雾霁霭靓静
  T:靨韃鞽韉韋韌韓韙韞韜頁頂頃頇項順須頊頑顧頓頎頒頌
  S:靥鞑鞒鞯韦韧韩韪韫韬页顶顷顸项顺须顼顽顾顿颀颁颂
  T:頏預顱領頗頸頡頰頜潁頦頤頻頽頷穎顆題顎顓顔額顳顢
  S:颃预颅领颇颈颉颊颌颍颏颐频颓颔颖颗题颚颛颜额颞颟
  T:顛顙顥顫顬顰顴風颮颯颶颼飄飆飈飛饗饜飠饑餳飩餼飪
  S:颠颡颢颤颥颦颧风飑飒飓飕飘飙飚飞飨餍饣饥饧饨饩饪
  T:飫飭飯飲餞飾飽飼飴餌饒餉餃餅餑餓余餒餛餡館餷饋餿
  S:饫饬饭饮饯饰饱饲饴饵饶饷饺饼饽饿馀馁馄馅馆馇馈馊
  T:饞饃餾饈饉饅饊饌饢馬馭馱馴馳驅駁驢駔駛駟駙駒騶駐
  S:馋馍馏馐馑馒馓馔馕马驭驮驯驰驱驳驴驵驶驷驸驹驺驻
  T:駝駑駕驛駘驍駡驕驊駱駭駢驪騁驗駿騏騎騍騅驂騙騭騷
  S:驼驽驾驿骀骁骂骄骅骆骇骈骊骋验骏骐骑骒骓骖骗骘骚
  T:騖驁騮騫騸驃騾驄驏驟驥驤髏髖髕鬢魘魎魚魷魯魴鮁鮃
  S:骛骜骝骞骟骠骡骢骣骤骥骧髅髋髌鬓魇魉鱼鱿鲁鲂鲅鲆
  T:鮎鱸鮒鮑鱟鮐鮭鮚鮪鮞鱭鮫鮮鮝鱘鯁鱺鰱鰹鯉鰣鰷鯀鯊
  S:鲇鲈鲋鲍鲎鲐鲑鲒鲔鲕鲚鲛鲜鲞鲟鲠鲡鲢鲣鲤鲥鲦鲧鲨
  T:鯇鯽鯖鯪鯫鯡鯤鯧鯝鯢鯰鯛鯨鯴鯔鱝鰈鰓鰐鰍鰒鰉鯿鰠
  S:鲩鲫鲭鲮鲰鲱鲲鲳鲴鲵鲶鲷鲸鲺鲻鲼鲽鳃鳄鳅鳆鳇鳊鳋
  T:鰲鰭鰨鰥鰩鰳鰾鱈鱉鰻鰵鱅鱖鱔鱗鱒鱧鳥鳩鷄鳶鳴鷗鴉
  S:鳌鳍鳎鳏鳐鳓鳔鳕鳖鳗鳘鳙鳜鳝鳞鳟鳢鸟鸠鸡鸢鸣鸥鸦
  T:鴇鴆鴣鶇鸕鴨鴦鴟鴝鴛鴕鷥鷙鴯鴰鵂鴿鸞鴻鵓鸝鵑鵠鵝
  S:鸨鸩鸪鸫鸬鸭鸯鸱鸲鸳鸵鸶鸷鸸鸹鸺鸽鸾鸿鹁鹂鹃鹄鹅
  T:鵒鷳鵜鵡鵲鶓鵪鵯鵬鶉鶘鶚鶻鷀鶥鶩鷂鶼鶴鸚鷓鷚鷯鷦
  S:鹆鹇鹈鹉鹊鹋鹌鹎鹏鹑鹕鹗鹘鹚鹛鹜鹞鹣鹤鹦鹧鹨鹩鹪
  T:鷲鷸鷺鷹鸌鸛鹺麥麩么黌黷黲黽黿鼉齊齏齒齔齟齡齙齠
  S:鹫鹬鹭鹰鹱鹳鹾麦麸麽黉黩黪黾鼋鼍齐齑齿龀龃龄龅龆
  T:齜齦齬齪齲齷龍龔龕龜
  S:龇龈龉龊龋龌龙龚龛龟
);

my $simplified  = join "", grep {s/^ *S://} @table; # corresponding characters
my $traditional = join "", grep {s/^ *T://} @table; # at corresponding offsets


sub subgen {
    my $translation = shift; # argument of the form "tr/鯤.../鲲.../"

    my $subref;
    my $code = '$subref = sub { my $a=shift; $a =~ <<TRANS>>; return $a };';
    $code =~ s/<<TRANS>>/$translation/;
    eval $code or die "$@";
    return $subref;
}

my $simple = subgen("tr/$traditional/$simplified/");
my $trad   = subgen("tr/$simplified/$traditional/");

sub simple { return $simple->(@_) };
sub trad   { return $trad->(@_)   };

1;

__END__

=head1 NAME

Lingua::ZH::HanConvert - convert between Traditional and Simplified Chinese characters

=head1 SYNOPSIS

    #!perl -lw
    use Lingua::ZH::HanConvert qw(simple trad);
    use utf8;
    
    my $t = "國"; # Traditional symbol for "country", unicode 22283
	# or: my $t = v22283;

    print simple($t); # Simplified "country", 国 (unicode 22269)
    
    $s = "鱼"; # Simplified symbol for "fish", unicode 40060
	# or: $s = v40060;

    print trad($s); # Traditional "fish", 魚 (unicode 39970)

=head1 REQUIRES

Perl 5.6

=head1 DESCRIPTION

In the 1950's, the Chinese government simplified over 2000 Chinese
characters, to help promote literacy.  Taiwan and Hong Kong still use
the traditional characters.  The simplified characters are hard to
read if you only know the traditional ones, and vice-versa.

This module attempts to convert Chinese text between the two forms, 
using character-by-character transliteration.

Note that this module only handles text in the Unicode UTF-8 character
set.  If you need to convert between the Big5 and GB character sets,
then please look at L<Text::IConv>.

C<simple> takes a string, converts any traditional Chinese characters
(such as E<22283>, unicode U+570B, meaning "country") to the
corresponding simplified characters (like E<22269>, unicode U+56FD,
also meaning "country"), and returns the result.  Characters which are
not traditional Chinese do not change.

C<trad> does the reverse; it converts any simplified Chinese
characters to the corresponding traditional characters.  Characters
which are not simplified Chinese do not change.

=head1 BUGS, LIMITATIONS

B<Transliteration is not perfect.>  At the moment, this module only
performs character-by-character transliteration, using the
(one-to-one) mappings from the Unicode consortium's Unihan database. 
Converted text is very imperfect, though it is generally good enough
to be readable.

The transliteration mappings could be improved; if anyone knows of 
another source of mappings then please let me know.  Ideally, I'd 
like to see the module performing word-by-word transliteration, if 
suitable data sources were available.  See
C<http://www.basistech.com/articles/C2C.html> for a discussion of 
transliteration issues.

The module may take several seconds to initialise.  Each subroutine is
slow the first time it is run, but is faster when run subsequent times.

The characters in this documentation may not display correctly unless
the program you are reading it with is unicode-aware.

=head1 ACKNOWLEDGEMENTS

The data used by this module is taken from the Unicode consortium's
Unihan database, available from C<ftp://ftp.unicode.org>.  Thanks to
them for compiling the data.

=head1 AUTHOR

David Chan <david@sheetmusic.org.uk>

=head1 COPYRIGHT

Copyright (C) 2001, David Chan. All rights reserved. This program is free
software; you can redistribute it and/or modify it under the same terms as
Perl itself.
