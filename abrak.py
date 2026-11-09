import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# ---------------------------------------------------------------
# A korábbi output adatokból csinálok szemléltetést kézzel
# ---------------------------------------------------------------
valtozok = [
    "Konstans",
    "Nem: Férfi", #ref: nő
    "Iskola: Érettségi", "Iskola: Diploma", #ref: nincs érettségi
    "Településtípus: Megyei jogú város", "Településtípus: Város", "Településtípus: Község, falu", #ref: Budapest
    "Gazd_akt: Munkanélküli", "Gazd_akt: Nyugdíjas", #ref: foglalkoztatott
    "Anyagi helyzet: Megfelelő", "Anyagi helyzet: Inkább rossz", #ref: jó
    "Akadályozott: Közepesen", "Akadályozott: Igen", #ref: nem akadályozott
    "Korlátozott: Nem", #ref: igen
    "Krónikus betegség: Nem", #ref: igen
    "Általános egészség: Kielégítő", "Általános egészség: Inkább rossz", #ref: inkább jó
    "Ápol: Nem", #ref: igen
    "Van gyereke: Van", #ref: nincs
    "Társas támogatás: Közepes", "Társas támogatás: Erős", #ref: gyenge
    "Fekvő idő: 4-8 óra/nap", "Fekvő idő: 8+ óra/nap", #ref: 0-4 óra/nap
    "Tehet egészségéért: Nem", #ref: igen
    "Depresszió: Van", #ref: nincs
    "Elégedett élettel: Elégedett", "Elégedett élettel: Nagyon elégedett" #ref: elégedetlen
]

MODELLEK = {
    "mozgas4": {
        "cim": "Mozgás (referencia: 4 = se nem sportol, se nem mozog)",
        "cimkek": {"1": "1 – nagyon aktív", "2": "2 – nem sportol, de mozog", "3": "3 – sportol, de nem mozog"},
        "coef": {
            "1": [0.4366094, -0.261475962, 1.0802684, 1.4703098, -0.3391315, -0.0730195, -0.5870918, -0.5385269, 0.1684288, -0.09568223, 0.3434897, 0.047779138, -0.7127859, 0.3824733, -0.3787033, -0.52095209, -0.44976184, -0.8021158, 0.25104608, 0.1903736, 0.5360204, -0.5000756, -0.02948987, -0.7261973, 0.02859487, -0.9061911, -0.5318763],
            "2": [0.9330924, 0.002992349, 0.1430352, -0.2721729, -0.4122403, 0.02631829, -0.2833263, -0.5593561, -0.1999804, 0.08444792, 0.290019, 0.009904156, -0.4908312, 0.1166852, 0.0961262, 0.08152483, -0.52300688, -0.518362, 0.01098436, 0.1108742, 0.333189, -0.338322, -0.55912427, -0.1096563, -0.0983334, -0.9129959, -0.567975],
            "3": [-0.4514106, -0.432042505, 0.5760452, 1.5473594, -0.862885, -1.2611889, -0.4674329, -0.7492137, -0.5403788, 0.09274155, 0.256286, 0.647435794, 0.5127219, 0.7285499, 0.2411377, 0.21883886, -0.01406947, -0.9232289, 0.54739423, -0.1496762, -0.1256488, 0.1265075, -0.04399036, -1.0912087, -0.68581713, -0.8287743, -0.6537532],
        },
        "se": {
            "1": [0.7449721, 0.2045635, 0.2384171, 0.299161, 0.3507242, 0.3248773, 0.3490685, 0.3833737, 0.2249103, 0.2547966, 0.3612765, 0.2688308, 0.3962047, 0.2388963, 0.4295832, 0.2538736, 0.37075, 0.2380819, 0.2242584, 0.3005326, 0.3234572, 0.2485265, 0.2389469, 0.2637602, 0.2390957, 0.502299, 0.4450486],
            "2": [0.6589288, 0.1782394, 0.2060618, 0.3256278, 0.3615517, 0.3297907, 0.3358793, 0.2980753, 0.1969202, 0.2403585, 0.3211285, 0.2221012, 0.320815, 0.2083269, 0.3747216, 0.240808, 0.3308314, 0.2211892, 0.19579, 0.2561569, 0.2761414, 0.2053865, 0.2238241, 0.1951008, 0.2015555, 0.4041724, 0.3674328],
            "3": [1.2236733, 0.3362259, 0.4271233, 0.452053, 0.527503, 0.5229248, 0.4950005, 0.6721329, 0.3787959, 0.417173, 0.5969718, 0.4210143, 0.6348086, 0.399354, 0.596348, 0.408043, 0.6645107, 0.3691466, 0.3467926, 0.4727435, 0.5217798, 0.3982683, 0.4077703, 0.4944276, 0.4487423, 0.8278694, 0.7395986],
        },
    },
    "klaszter4": {
        "cim": "PAM-klaszter (referencia: 3. klaszter)",
        "cimkek": {"1": "1. klaszter", "2": "2. klaszter", "4": "4. klaszter"},
        "coef": {
            "1": [1.2515065, -0.03508794, -0.006351858, -0.05795199, -0.22154351, -0.4803121, -0.6119556, -0.3096278, -0.255222, 0.04649799, -0.1288724, -0.03079408, -0.6207062, -0.0866141, 1.00276291, -0.1627048, -0.6318932, -0.03616746, -0.06830902, -0.02482363, 0.3547788, 0.03253885, -0.04960076, -0.1208552, 0.08671259, -0.2746981, -0.2583613],
            "2": [-0.422096, 0.03674139, -0.096425585, -0.41594498, 0.41806079, 1.2147876, 0.7869879, -1.180853, -0.2186809, 0.47009076, 0.6824596, -0.55585636, -0.7694091, 0.1881249, 1.08554849, 0.1864703, -0.2776322, -0.43252547, 0.03096473, -0.02923356, 0.4648362, -0.40116957, -0.59419726, -0.0548149, -0.12395114, -0.981592, -0.4664459],
            "4": [-0.2175302, 0.04208676, 0.512532037, 0.85299126, -0.02678967, -0.323805, -0.3795153, -0.247688, 0.2581013, 0.04637137, 0.6156313, -0.05192028, -1.3671332, 0.1087095, 0.03139097, -0.5792259, -0.4265355, -0.41341026, -0.12709043, 0.10167711, 0.445676, -0.44233656, 0.33426529, -0.9978112, 0.09163658, -1.1086364, -0.4730693],
        },
        "se": {
            "1": [0.6464521, 0.1747217, 0.2062671, 0.2799312, 0.3186176, 0.2952793, 0.3032154, 0.2765708, 0.1974452, 0.2297318, 0.3120063, 0.2161537, 0.2891352, 0.2123999, 0.4701603, 0.2383054, 0.3187473, 0.2206262, 0.1901684, 0.2463567, 0.2693778, 0.2061293, 0.2112153, 0.1968837, 0.1989959, 0.392898, 0.3721426],
            "2": [0.8234322, 0.2098381, 0.2439825, 0.3553793, 0.5232318, 0.4794982, 0.4878265, 0.4002178, 0.2299946, 0.2887317, 0.3838531, 0.2795081, 0.3755201, 0.2459616, 0.5042134, 0.2844245, 0.3928911, 0.2483514, 0.2294134, 0.3038612, 0.3274061, 0.2460137, 0.2635263, 0.2324155, 0.242698, 0.485596, 0.4344329],
            "4": [0.9588082, 0.2710187, 0.3242894, 0.3861178, 0.4305251, 0.4138692, 0.4358408, 0.4773702, 0.2962226, 0.3370905, 0.4564276, 0.3457018, 0.560446, 0.3159863, 0.6997931, 0.3291809, 0.4806339, 0.3062642, 0.2970651, 0.3968775, 0.428988, 0.3416421, 0.3044254, 0.3854913, 0.3164653, 0.6448226, 0.548139],
        },
    },
}


def or_tabla(modell):
    """Oddsarány + 95% konf_int"""
    sorok = []
    for kat, coef in modell["coef"].items():
        for v, b, se in zip(valtozok, coef, modell["se"][kat]):
            if v == valtozok[0]:  # intercept kihagyása
                continue
            sorok.append({"kategoria": kat, "valtozo": v, "OR": np.exp(b),
                          "also": np.exp(b - 1.96 * se), "felso": np.exp(b + 1.96 * se),
                          "szig": (b - 1.96 * se > 0) or (b + 1.96 * se < 0)})
    return pd.DataFrame(sorok)


def abra(nev, fajlnev):
    modell = MODELLEK[nev]
    df = or_tabla(modell)
    kategoriak = list(modell["coef"].keys())
    valt_sorrend = valtozok[1:][::-1]  # felülről lefelé olvasható

    fig, tengelyek = plt.subplots(1, len(kategoriak), figsize=(5.2 * len(kategoriak), 9), sharey=True)
    for ax, kat in zip(tengelyek, kategoriak):
        d = df[df.kategoria == kat].set_index("valtozo").loc[valt_sorrend]
        y = np.arange(len(d))
        szinek = np.where(d["szig"], "#c0392b", "#7f8c8d")
        ax.hlines(y, d["also"], d["felso"], colors=szinek, lw=2)
        ax.scatter(d["OR"], y, c=szinek, s=35, zorder=3)
        ax.axvline(1, color="black", lw=1, ls="--")
        ax.set_xscale("log")
        ax.set_xticks([0.1, 0.25, 0.5, 1, 2, 4, 10])
        ax.set_xticklabels(["0,1", "0,25", "0,5", "1", "2", "4", "10"])
        ax.set_title(modell["cimkek"][kat], fontsize=11, fontweight="bold")
        ax.set_xlabel("Oddsarány (log skála)")
        ax.grid(axis="x", alpha=0.3)
        ax.set_yticks(y)
        ax.set_yticklabels(d.index, fontsize=9)
    fig.suptitle(f"Multinomiális regresszió – {modell['cim']}\n"
                 "Oddsarányok 95%-os konfidenciaintervallummal (piros: az intervallum nem tartalmazza az 1-et)",
                 fontsize=12)
    fig.tight_layout(rect=[0, 0, 1, 0.95])
    fig.savefig(fajlnev, dpi=200, bbox_inches="tight")
    plt.close(fig)
    return df


if __name__ == "__main__":
    abra("mozgas4", "mozgas4_forest.png")
    abra("klaszter4", "klaszter4_forest.png")