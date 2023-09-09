# Level-Difficulty-in-Game
Analyze data from the hit mobile game: Candy Crush Saga

<a href="URL_REDIRECT" target="blank"><img align="center" src="data:image/jpeg;base64,/9j/4AAQSkZJRgABAQAAAQABAAD/2wCEAAkGBxISEhUTExIWFRUXGBoYFxgYGBYaGBgeHhgaGBgaGRUdHSggGB0lGxgZITEhJSkrLi4uGB8zODMtNygtLisBCgoKDg0OGxAQGy8lICUuMC0tLy0vLS8tLy8tLS0tLy0tLS0tLS0tLS8tLS0tLS0tLS0tLy0tLS0tLS0tLS0tLf/AABEIALcBEwMBIgACEQEDEQH/xAAbAAACAgMBAAAAAAAAAAAAAAAFBgMEAAIHAf/EAE0QAAIBAgQDBQQECgYJAwUAAAECAwARBBIhMQVBUQYTImFxMoGRoRQjUrEHM0JicoKSssHRJHN0s8LhFTRDU2NkorTwVKPxFiWTw+P/xAAbAQACAwEBAQAAAAAAAAAAAAAEBQIDBgEAB//EAEIRAAECBAQCCAQEAwUJAQAAAAECEQADBCESMUFRBWETInGBkaHB8Aax0eEUMmLxFSNCNFJykqIkNWSCo7LC4uMX/9oADAMBAAIRAxEAPwC3kr3JU+SvMlaGMjEOSsyVPkrzJXo9EOSsyVPkrzJXo9EOSsyVNlq3wzhjznw6KN2O3pbmarmTUSk4llhFkuUuarCgOYG5azLTk/B8JBGXly2HtPK9h94UUJk4rwnkYmHVBKR+0mnzpaeMSAbJV4D6wyTwaeRmnz+kA8te5KO4LC4DEm2HnXMNwkge36UZuw+IoHEQSRe9iRfrY2vRlNWSqgHA9s3DfbwMCVNHNp2xtfYvHmSsyVNkrCtFQI8U8SnhazZdDrtbTrWnD0+qj/QX90UTxPCnbDvKQAmW4BuSw20At94qCPBPFDCXHheNcrDUeyDY9DbkfnQMuolLqlBKgeqkd4Kn7fOGEynmoo04kkdYnuIQxjTJXmSplQkgKCSdgN6PcJ4B4lMxG4OQHzF7nnvy61ZU1cqnHXN9BqftzMU0tHNqCyBbU6fvyELeSvclR4GTOoNW8lFQLFfJWZKnyV7kqJiQiLJXmWpstZlqoxYIiy1sq1IFrYLVUy0WovA/F4oxXbXcAHYjcgg1BH2lcqxZVJuNbC/TxHn76J47CGRbDLfowup9baj1pbx3DlEijXXUooJsBYGzWOm2ttLisrUyFy5pw5F233PrG+4VUUtRJCJ6RiTqcmDAXyGXzjO0vGDiMKUyAubWK2GzA6jbYHavOw7NGhBS8gkEjX8fhUCxAU3Y72AB1t5U44vg0LRpkWKIOoBORXYg2sA21tL5uelBeJ9nxhnj7pmLOSSx0AVbH5f+bVBaVpQQfHX33QYkU8xOFLpdy2abAlzbkclNHTOBzM8MbuuRmUEqd1vrY+dFFNL3ZPFNJAHa9mZil9yuY5Wt+cPEPIijymh8iRCIxLQqTtJg1JVsTGCCQRm2I0IooDXK/wAIHZxMOVmRmIldswa2hOuhAGm+9G0UiVOmdGskE5N3mAqudMkoxoALZw+//VGD/wDVR/tVlLPB+wEMkEbvLIrOgYhbAC4voD5GsqSxw5CikzFWtkIilVYoAhCfExRy1mWpstZlrZRlohy1mWgvFO0iwytEI8+UC5zW1OttjytUPCu1Ku2WVQhvoRcjyB8/P7qpM+WDhJ99uUXimmlOID32ZwwZazLU2WtWFXRRGuFwplkCDQbseg/mdh/lRrinFBhlSGFA8z3EUd7AAe1I55IL6nck28xH2chtGX5uxPuBKr9xP61COCy96ZMUdWnYhD0hUkRAeR1c+bGsVxivKpiiMklkjR9T6+A3jZ8LoRKlAaqufp3ffW1mHhClhLiG+kzDXO/sJ5RR+zGPmeZJraXtHhEJVsVCpG4zDT4bV5w/AfT3cuSMJE5QKDb6Q6+2XI17tW8OXmQQdBZmyDBRxqESNEUaBVUAD3AWpKmgVOGOaouffYOxreQPVOCThSIVcNJE+PwTxlGB727JlN/COYpd4GboGPPX460/R9n8Ms4xCRBJRm1Twhswykso0Y25kX86CtjuE4Y9yAGKaMESaYqRyZlDZT5XrRcJny6KWUlz4bk6nnCniNMurIwMM835bCB5AqCZ10ufDm8XpcZvlemLBS4ABsXHMvdKMsgPiVSStrqwLo2wsftcqjx3H+FSoyPLHZhY5UYMPQhLg01XxWWXSEm+u3dm4hbL4TMSQokOC7btz5xR/CF9IvEsV1iytqASpe5FnA5BbEDzPStoVkThDDEE3QAKW0Y2dRHp15elHeAccw0wEUEpLIo8Lh0cqLDNZwCw21HWoO1kuDCxjFzZAHzrGDrLYWsYwCzgX/JtalMqcgGW6WKSCSMyz/X9hDaZJWcYxE4nsdHbv0+fbATh/FTFDEkMPeYmYOw5Kqq5XPI/JRoLDU+WpEvZzCPHxFu9lMsrYcM7HRfxosqLsqDkB5k3JJqrhMbBJjY/o6OsSYaRRmjkQXMgawzi53vVkcWhw/Ec80gRThQATexPe3toDSmZUrmVuF7Fz29p5bcru0GolhEkWytAHgY+rHoKY+G8JMoDsSqHaw1PnrsKoJJwiNLDFuQBte5PlpGL3taj/C+IQYhWZZg+QAsqFlWMG+Ucs2x1PQ2ttWmrOJnCEyLbkj5P4wipeFAKKp99h9fp4xHJwCO3hkYHzysPgAPvoNisO0T5HA11Vh7LDqD/AA3FMsi2F485/NIchvIE+yehGnUGosTFHiI1uJMtw6lQQw0sd1O43FuQ6UJS8VmoU04unsuPCCKnhklaXlBldtjC/lrzLRbEcIGW8JYsPyXIu3kDlFm8jv8AeLia/rsQdx1BFPJFTLnpxSzCSfTTJBZYjAtehaky1sFr02PSs4ikBym29AkhxLxFA8StPJGVbNlI7ttFKnVlO+nNiNNyxldKocN7PRSDOWkV1AN1a5Db6BgbWPLakc9akzgUgO2vaX+Uafh0pC5SukJA5dgYe+bbxv2qMEeFTCGRgVEVsou1o2VlJ1Fr5BVbG8WE4EncmNFRwzsfCFa1yLa8jt1ohwrs1Cw7ySSTENfV5AFznqFABA5anlVrikAZhGCq3MYUHayyI5Gm2gIoSbjUkkkNo31hs8mWkpSglbXJtyZgdL5888gQ7HiXuS8pYZzdEbeNAAFB/OIGYjkWI2FMiNQbCzggEHQ6iiEctLQtzAi0xcIuCLkXBFxuPMedcO4pjJWZkkleQIWAzMzbEi9id67U+ICKWY2CgknoALmuEPJmJbqSfib1ouBhyskZN43jP8ZLBAfN/SL0fHcWoAXEyqBsAzAAcgBesqhesp/0aNh4CEvSL3PjHRMtUOM8QXDxGRtTso+03Ifz8hV2XExqbM6qbXsSAbDc23tXNu0fFjiZbj2F0QeXNj5n+QqM6bgTbOJ08kzFXygc0pZyzG5Zrk9STc1I6W1FQCrZpWYciH3sxje9hH2k8J9OX8vdROceE0i9k8f3UoBPhbwn37H3H7zT/ItwRTOmmYkX0t77oT1crBMLZG/184vcNP8ARUtv3Xzy/wA6Wuz0wGGwoXbuUt+zR7s3P4WiO8bEj9FiWHzzD3ClcxGBng2MLXTziYkxkeQ1T1Q1ga+QUrUg6EjxyPZl4xvKKYmYEqH9Q9iHTsKw+gw26OG/S7x+89+fNTAK51wDjn0VmuC2GkbO2UEtA59o5Rq0bHU21BudQTZ4wnFYJFzxzRuvVXUj43opC8YxD9uXvtgaZLKFMqKHbWaVcI4iEmZyqFo1dnRCfrGATxAhbgEbEg0u8P7Q4CKFFw8g7vMqARqbgnS7D13NWO1HHI8Q6YWGYNGczYh42uMq2tFnGgLE6gHZbHeh83E4YQsVgikCwVCUUE5VzkCygtYC+5oKuAmKShiSLkD9je3cIvp0HCVFm9+UU/wgR5Iu/jsjs8cMttpFMgIzDmVYXB5XPU0z4/FxopeQDKljsCb/AJNhzN9qTu2mILYUg7ieH+8FWu2GIzQW/wCPB/eCgzLxIlpVkCR3dX7wSZYSVQV7XYqSLuJ4lDzxzBUB0BDgowYjdbG9vKt8Nho8KrzzyBpTrNO+rE9F+yo2CitOIYkFo78ph8crW+dQzYtDiMG0tu6WZsxPshzGwhL+We1uhseVekhS0pkuySfY8rDUmILRgBW2UXcLxtZX7vJOjZc472J4wy3tdc2+taQH/wC4H+yD+8qftVLlx8R/5Z/7wUNw2KH00t/yv/7KhU06ZailOTekclkrlgneCsBH0ybQW7mLl+dJVTCI5xOJZMt3kihRSPCGSIyF3HNVVyQo3Itpe9eR4sDEytyMMQ/6pKn7Ntmmc9cYf+yFX8OltO/5fpEJqWS59tF7ivBI0w8rR4dJpxG5UyKGaR8pIzHmS3LTppSn+DjBtKZlmi7yMKjB5cOsTLI1zJGBbxKvX+FPXayeaPCyPALuANhchbjMQOZC3PuoZ2WGds6Y550yeOOQeJWNrGx1Ub6fM07dgRFQTMKOlxWdsz6WHJyHY7RO8TQOqg3jYNlBvmQqpbLf8pCAbX1XzGgr9ogB3cthdmKMebeG6k9SMrC++1EeML4ovWT+5ehnaj8XD/Wj9xquoiU1KG1ceRgOuAXTKft84gUVvasjGgrYJfQVpJgtGWQbxJHhydRYgHW2pHqOVScNkABY2Bez2HK4GlKnHMVNA6urELqCRsGJuwta4vodaow8el1JGYaAW5AcgedZhdScfXDKFj5xvqPhh6LFKW6FXG+n7x0Y4hetBuMYwRusgVpCwyIiLdma+c67KLDVjoPvXYuOsy3ItRbAyFmgZja0Ush/XeNVPwjf4muGd0gbvjtTTqpkPuW8f2gzwhGjijVyCwUZrbX3NvK9Eo5qFR42MgkSKQDY6jwnoehqZ8Sq2DOoJ2uQL+nWlcxCnfd/vA6SGY6Rr2kixE8XdQlVDe2zEg2+yAAd+ZrnfHOCvhSgdg2YEjLfS2+4HWmfiva5FOSAd6+19cvutq3u+NLmOw2PxLBnhmbp9W4UemlabhUuqkpHSAJRc3YKPO9/FuyM9xRVPNJ6N1LycOQPT590Bs5rKI//AE/i/wD08n7Jr2nX4iV/fT4j6wm6GZ/dPgYAYrFNK7O5uzG5/l6DaoilYBUi0vaGuKIstWa8CVtaukR4KjVdD610ngGN76FSfaHhb1HP3ix99c7gW7eldD7PQZIE6t4z79vlar6Vws9kC1hBlh83iTEl4nEyC5G4+0p3H+fUCruPwceOjWWJ8sqXyOR7N7Zo5F5qbC48gRyrGW9DJcG8bd5C5RudufkRsaH4jw0VHXRZWuxHP67Wi7h3EugGBf5fMdkB8QrRPlkBw8vRj9W/nHL7Lg9NGHMCtZMHmN2w8Tn7RRCfjTCO0jgZMRhg455bWPrG2nzqkcVwnc4C3kIkA+Aa1ZmZwueCxQfB/Ai/iSeZjUI4tKUm6kntsfAj0EUzjRGArWLjaONSz5dmORATYb32FqjMccuWQZJALZXDaWvcX11sddaeeyWIw0kZbDQiEBirKEVDcAHXLvoRzrfGcG4azlpYcLnJ1zCMEnzHP31T+CKSzkHW3pp4l3vExWhV8IILNHPGEcysjOkgzAkBwCGU3HzrTjZJiJJBvNDoCD/tVrqknBcJIq5sPA6gWW8cZAHQabUrduOBYVUgijw0MZmnVS6IiuoQNMcrAXBJjC+hNcNMEjEVZXy27+US/GFVsIc2zhe7RLmCr3yREzx+MkWTxe02o06+VbSTEM0MyqkuW7xMQUdftxts6HkR8jcDzi3HsPhZFheMkFQTlAIUHQXB1O1UpeOYHCeGIGTNZiy2OVTqoudgFIAUbCh0IJlBOAkm6Ta+h9PraCDj6TEDYi4Y6PlE5wqRAsAsYC5cxckKu9lBNgPIVciZFQOXW5GQPmFrE3UX9fvq5gMLEMdhJTGjd7njOZQf9mZEcAjRgVtccmNOMnZbAs2Y4OAte9+7XU3vci2utWy5PTICyo35dzXPvaKZlT0ZwBIbSOezO4B1U7XtvbYXIph7Eam5/wDVn/sqD/RommxJgjRIjKEQIoVTkUK7AAW9vNr5UX/B+2YlvyWx0mTzC4Vk/eVqtpEgTlJGn29Xj1RNxyQSGd/kYc+LicRMcOEMumUOSFOovcjyvbztStwrheKkxi4l4I8MioylUdD3pNwScuhuSDc6+Eb8m3jWPXDwSTNsik26nZV95IHvpU7D4maORsPifbmUYuO9/wAvV112N9bcrNTEi8DySoSlqDbXdyNWuxazuC1iGIeDHGl8UXrL/cvQXtR+Lh/rf8Bpg42vih9Zf7h6X+1HsQ/1n+A1OmH+1I7/AJGA6v8Asyuz1iNDpSH2l49JJN3MT5FBAzXtc5su/IDn7+VNvGMX3cJsbMQQD008Te4fO1cvGHaaVY0HikYADp/8Ci+JT8a+iGQz5n7fN9oJ+GuGgSlVcwf4eQ37zDtxPiyABMS1wSAbWzqyrbPYmxBANxpqfOquKwbJ9F7lWkXFWERAC2JUOAwJ08BzXG2tJ/a2fD940ECi4cqWHRVCW88zhmv0y06YztHCmO4Uha0WGwveNb7TwkWt9rwIAPzqGk0aSkGYdCdiwFsvDui4cQmS0BMndjZwSdAN2uWzcPe5IcU4SMHAcTiGBAUmOP8A3jWuqkfZv7XlehPB+1EmMRblFKRKHygBmtpc+QYk2GgzedLHbLtDLjpHdtFAIjXkq5tvMncnmfKwEn4PMCkiYnOiuVWMKG9nxyBXN+oUEj1qP4cKkko3A995i6dNVKU0+6ignfDfQCws7kDM20MTYHE/RsS4Vu9jvkkHKRCASCOtj8RTth+Ew+EuzT2AMdzZAhuU2N2O99QLgixtSLxXh30bEvGBZfbT0OtvdqP1acuz2IzQ5L6xjOvnG1s4/UbXyF+tUibOloPRKIfbP7PlbkI9xmiTPpE1AYqSL8x9vvBuKcqLJZB0jAUe/La/vqNjffWsrKUnrFzcxkySc48tWV7WV6ORzECtgK3Ar1RW3aKiY3TpWFa9tXtqlhiOKJcHFcgXsCQL8gL70747jcULiOxNhrlt4eg89P4UjRta9eZzUkqKHbWILSFkYshHRMHxSGU2Rxfobg/A7+6rlcwSdlIIOo1B5iuicKneSJHdcrEXI+4+VxraiJU0qsYFnSQi4yid41O4qM4eFI2xE/hhXXbV9bAAbm5sABuTUjJndY/tHX9Eat/L31rxNe/xkcP+yw6CZhyLtdIh+qoY2/OHSk/GuIGQno0WLOTryA2J+R5w24RQiZ/NXcOwHzJ7NOcD5++xABmLQxH2MNEcthy7511cnmo8I21tc1W4bhIhlMWFjHQhAfnRjGxSPKmGibI8gLySWuYowbEqDpnYmwv662tTFw7s7hYRZIEJ5s4Du3mztcmsnKlLqE41KYaD7Pl2uTflGnVMRLskPCbheEonjwzthydc2HbwE9Wj1R/1ga94/wAWmaALiUXPG6yQ4mMHuyy3GWVNTFmRmTMLqM1zYCmXifZSDWSBhhJB+WgAjP8AWRXCuPg3nQdsZNCcs8IkG3eYZlkVvWH2wT0AI86tKaiVl107HP32eEVlcpf6TCl2nwa42FMdhfHZbSKNTb06qb6etBuzHZx8W4uhTDqbsxFs1tco/if/AAPGG4zw6Ny0aushvdUilDE7eKMafEVYxHFYWTJLh8QqE3AVTYjfXu2uAfsmq0TalEsoRLVyJFwOzItobdkFprClGDEGPZ3t2xHw/i2FGJ7+SZEhwwZIhe7SSkZXMcYuzhEuugNy5+yav8S4tiMYCkavh8OdGdtJ5RzCKPxSkflHxWP5JqnhJMPEpbD8PmHPN3PdL697LYD1vUb8SmaXDgmARSy92yxSiWW2Rm8Tpons28JO9eC5yZWGUgpAGajfc23z35ZAwIcBW6i5OkZiYNYsJhwEaQ90pFssShSznzYIpIXrba9MvA8IkMkcUYsiYjKo8hhGAueZ86G46WOPF8PuVjQPPuQqj+jvzOlFuHyA4gEEEHEkgg3B/ojbHnRXDAOiCt3fuJHo/fEahRKu6LvangjYsRI0gSFXzyjUMwA8IDbAam/uPKhEfZjDd4kuCnTvY5Ax+sMgK6hlIBJW4Nr+tb9vXaSTCYTOUjnkIkYaXAyAJfzzH32ryXC8MwuLgj7po5gVyFe8ykuSi5zeza/+WpkWJiyWZiZSQFKuFEJSHDXBxXGZBBsWAd2YQc44vii9Zf8At5KWu0qkrCB/vP8AAaZ+Oe1F6y/9vJSV204uIogo/GfknoStifctz7xUOk6Kclez+oHmRA6aZVUkSE5qYf6r+UKfaniOdjGpuBp7hr8zr6BardmMGwBnXSSUmLD9Rzlm/VW9j9ogc6p4bBmV+6zZbgvM50ESDViTyJHwHqabsOAsT4nLkCx5IFNwY4h7NxydzZjz9kfk1KXLKsS1ZAOfTxPk8OOLVCKdEuhkZkgfU9wEc949GhxJCKAkaiNQANgMqnzuoY/rCqwwqZiwe7kAFcuwGY3zX19Lfk1ZgjzNH1aTN7s2QfJc361XHIZQBIDZCSojVcuhHtjVt+dQWSCEjS310OvZB8uSlCRh3fyIB8BeAUieBj1y/NhT12R4akMqgT4aYTDJkRsxjkXLLGXQjYMgN+tJkqfUynpk/evVzg8xjxCPf2ZFJ9CwB+RqwlpQ7T/4xRUU5mrWAf6Q9ne6znoxvDj2mwJniEqg97HeRepG8inqQdbeooVwXiPdlHXXKcyjqDo6Hrz/AGfOnGZbSOBp4gyn9MZj7s+ce6kvjuEEEmZfDFI1x/wpPyh+idx7ulBSyLy9suzTyhfwOsF6Wbpbu9+RMOgt+SbqQGQ9Ub2T7tVPmpragfZziGZDE3tR3Zf0DYuvu9oeQPWjlCzUsqM/xKiVR1CpRyzHMaRlZWVlVQBHOQK2ArYLW4Wt4EwIVRrapAorAtSKKnhiGKIyteZamy1rlr2GPYov9nOHLNL4vZQZiPta6D0vvTzekngeJ7p8x1Gxtvb/AOaYcXxEDK6ZSCbXub+hFqulAAQPOJJ5QX4VriD5Rn5sv8qq8Nf+k44nfvYl9wgS33mq/DeLR/SlF/bBTyJNiLe9be+pcce5xr39nExhlP8AxI/Cwv1KFSOtj0rHcfQozpj6hJ7gz/I+Ea3gxBp0DtHn9xBDs5Y43GMT4guHUeS5XI+JJ+FNYrn3+kfo84xQUsjKI8Qqi7AAkpKq7tluQQNbE2ubCnnBYpJUWSN1dGF1ZSCp9CKFpiDKTyAHeA0GTkFKyDAX8IgBwEtwCM0Oh1H4+PlQfGdmMI9ljiSBw6sJI0XOCrZha+g1A+FFvwiOBgJLkC7wgX5nvo9B1NAeL4xitkdkLTQxlltmAeUI1iQbGxqitSorl4Sxc/NIeJyEgoUTpG/DuB4UQuGhR2UyjO6gsbM2pPM1R7N8IwrcOjdsPEX7ljmKC9wGsfkKn4FiT3FmYsbzAk7mzMLnzqHs1PbARr/wG+5qAmpV1gSXxc/1QQlALNt9IzsrGuOghmxC5440SPDwt7C5ECtIy7M7MDqdhp1vZxq4VpsJJAsNxMVLRhbjwNoSKpdlC2Eihwz7PGs2HY/lqyqzr+kjG1uhB51VPCRDjIZYQBE8pZwLgq2VvcVJNxfUG/U0TUIKqheNTM+HYi9u8a753vFUtP8ALSUjthsxmFgkMbTKrZSVjD6qWa2y82spt0BatezOGjikyxqEU4tmyjYE4Vr2HLXX30K4rPfEYKx07yS/7FFOCSgyj+0t/wBq1d4eClYvYg/P7ecenpsfekNHG+FR4uPu5LixzKymzKw2ZTQPhfZ/vZEkk4hLiUgl8K2As8Z1BYkkkEann1q92n4v9GwzyX8Xsp5sdB8NT7qWexOOTD4gQCcSrPGrkjXLNbxLtta+vPw05Cg8RkommQopyDtZ9OtduqwIfJ797vxs+KH1l/uJK5F2lw8srvIJALKSoPlcmx66fKuldsMZ3aI350i+maGQXrhfEnaWQqzMAz+zmNjc2Om22lRmdZQYs31+gME8PlzQkzZRAbfmSIZuzWER2MCsXQWkxEo9mZr+CMMfaQEFiRuR63JfhAxeTCkDd/uGZj9wHvotwiAL3thYZ8oHICNFjsPepPvpS7eFnxMEQNxcHJoNnuzEnkFQaetNlS0y6NCf7zEnsGL0HJhpCKhnKqOKmas2Q7fL5/PWBWEw+WdF+wqr+ygB+YNaYOE2f9BvvFWhxPDpKWXNK5JN9kFydja7/KrbcYw6rfJZ7MAqIdN8uutxtcH7taCmUdSJXTFBZ++/6cxtcC7bxqzx2iQroUFywvkLZ3PvnAN8Ofo+IHoPkwqrgnRyoDpdkGlxe9trdbgaUSk40bENDGQdwoyE26MOh6ioVw0b4ctESyI+qMPEt91NtD+SQRyJq6fRzqYNOFt0lw+uj+IHbFdPxeRWL/k9VR0UOxsi2eebbGOiRzZlgf8A3kWv6uVx85X+FVON4ZGjbOLrbxctBrcHkRvUXApgcLBY6K4UehWS1/gvwqfjv+ry/ov+41JJlpo98vpGcXLMviAAtcQtwwxYWWHJM5cPbu1AkZ9CQFA0sdtdNQaaOEyloUZlyki+W98oubLfnYWF/KlPshEvf4YhbEstzbfxf5U1cDN8PCesaH5V2fYNnz8ffsQX8QSlyzKC1YrE5ZDb3lF6srKyhYzsIQWtwK2C1sBX0ICFpMeBa9Va9tW61JorKo1y0Z4f2febDyTIbshsE5uAoLletsy/OhINMXFMVJhIcEIzldVaY9D3jWAYcxlW1qDr55p5YUM3H1PvwhtwTh38Qqehe2FRfm1vOFtDV/B4OSRXdFOSNS0j8lAFzc9bchrRxe2ELHu5cEgzLmeJY/riSuZiuxvfUaXtrfpsvaJsRh8WiokMC4WTIi20uyKCx5mzHe2/voVXFg3VQXdrwwHwvUJSZk1QwhIUWZ75DM5+EKWJYkAjQixBG4I1Bp3wWIj4nhsjnJMhBJHtRuPZkXyOvqCymkkio0aSJxLExRxsR9xHMeVE8RofxCbWUMvpCbhtd+HLG6Tn9YYJ3ljk7uUCObkf9nMPtRtsfNTqPheoYQpLBJImJuxgkkizHq2QgGiuD7ZwTJ3WOhAvv4c8TeZXUr8/WiGF4Nw+XWDEMoPKOcke5XLZfSwrITaSZJVcFJ7/ACI05RspNbLmpuyh594hamw6mN3Ku7gWDyu8ri5FwGckj3VbxYa63Gn0nD/36UexfZvALlE80vjNlL4h1uVsxtlKr8az/QfDV8ffM5T6wL9KZrlPGBlL6m6jSoJkLKgSdeZ1GXhHVVCQlSQlg3hC7hCyqy2N1kmRuoPeMNvnU2HQr4FFlVMtha6gggEj50VjOAxsjSiSbCStbPZ0TvLCwYghkJtpcWbQX5VDjYOCxOsTOwkNyZkkkLX6SSqdfIEWHQVNdEszFA53LMXve+2efe144iuQEJLcnceUD5xPJDDBJKhjhKFMkREgyABfGWNiRoSALgnrWTYsB44ybPI+ZV52UEk26a0w4fs5gpBdMXKw8sQD9wqrxPA8HgCI72fOCGWVzKSbpd5A2bKAx3NhrUTImTT11Obsw+w9e7XwqJctPVS2WZgHjc3eYW41zyfuGr/BsSySNoSUlSWw1JBjeI2HO16tycM4RcE4s3XVT9LckG1ri7EbVkHZXBYk5k4hNKBpZZICfS4jzV2XIWgglsjvuT6x1dQlThtX8mgpjZY52jeSGdu7bMF+rCE8iyswva2leY7upcn9GlQo4dHV8IjBhtr3u3latMZgeGcPhJdV0BIVmLySHkNSSSeuw9KWsJ2wgdgo4aoJ2+s//nTKVJmTA6A4984CmVUuWRiLNlc29/WGDtbxTvICvd5RmB1eJva8H5DNbRjvXI8O2fEx/nSL82/zp37VcSDYbMIBBeQLYG97KxvfKOdqQuHt9dEejp+8KgUlJUDY5eUPuHYfwhUnU/L7x1zh3+1/r8R/fPXKu3XE2XGq6nWMgj45rfK1dVi8In/rZz8ZHP8AGuJ9rJM+IlP5xHzp6sYjKS1ggnySPrGMoOoKhf6kj/UVegizxGcRynL7DWkj/RcXHwuR7qjHEBWs0ROChkN7q7xDzX219wOb41Bg8EXSV8wHdKGItvchbeVHUlcpMkYjkcL3zBw+dvGOz6YKmWGd27njXEY7TT/zWjkOLEDxYY/7stN5s41B/RTLQrs7hRLioI22Z1v7je3yqpxCU/S5XJv9a+vUZiPuoSvUqoWJXJ+/ID5+UF0C00yhN5t3f1eRjpXZVrYdlP5Eyj/rjH+M0Y47/q0/9Uf3SKAdmGvBKessR+MsNHu0B/os/wChb4m1ZNQ/mjtg2sS/FEjcj1hc7Fj+lYUfnD940x8C/wBWg/q4/wB0UudkXtisMejL+9/nTPw5csSL9kBfgbfwqVQLPzHrBXxSGmSv8PqYt3rK0vWUJGVhPC17arL4cgXGo69PUcqiy19BlTETU40EEHUQnU4LGIjT9h+zmGyYd1UsSUZyWJDK2912tmsLDlStwCNTiYsyZ1Buy2voATcjoNz6V0psWgTvAbBRdSo8LDoCNNdNORpXxWrXJKEoJGvbo3qPWHHB6RM7EVAHQDV4HcU4XBJIs0iZiqlREoALEMbFyNSAOXnz2oNlkxOKV+6Xw2VRYZVVb5dNgbknyHQkUywyhIXxMhXMTZQzZF8gW1sPcdBsTRTCYjvI0cbOqtvfcA7896Trppk6WkLmHC2XvcRppM2VTzZnRp6xsT8x2BgDk4gdj+Cxt9YiIMQFsJcoz7bZ9/n5Up8GSfDTuViIZxlZMt1vmuct9lNr6bV0KvCL6VyZQIUrEglJZrcsvkPBsrROTVmUlSSAQRd9s4Dp9FkR1bDxq5Vs4yKdArHMptyIHvoHxLszhEhiJR45XMSsFYmzOpZlytf2cpPuq/x3HJDiEERUvGbt4/FewYju7eyVYeK49q1uYLcbx0UeHaQgSNYlWKjxSsuWNU/ONwNNgDerelnSVFAWQfLLbI7m1mJgCfSSJqEzEoDFxfQg73szNflmIQMb2Mb/AGciv5MMjfHVfiRS1xHs88R+tiKdCRofQjQ+411NL2F97a+vOt76EcjuNwfUbGoSOPz02mpCh/lPlbyEJ10CD+Qkefzv5xx0cOApj4e2HYgMuRibbXHx5U08Q7PYeTW3dN9pPZ98Z0+BFLXEODTwK3hDod3UXsPMWzJ93nT6k4nSz2Sksdjbw0PY7wvqKadLubjcX8dR8ucWcZ3JiZksbHLtzO3upaOGU6nU1IJjYgHQ7/wqMvTEiBMRivJw9DyFYmCQchUxesQFjYW95A+Zr2ER3GWiA4NelRvgVqw5IJB3Fa5q9gEeC1DKIY8EAb86J8IVRMl9gb/AE2+VUs1eZq6EgZR4rJzMXe12Pz4eE2tnaRgOgWwH3/OkpcWsbAlrEMDbfY9aOdti5jwSICxZJLKoJuS42A32qpwjsHiJZokxH9H73MVVrd4wUFmyp6Dn8Kz0yVLKlTFqzUq3YSPTaNxS8QXJpZchCbhIueYBsB9e6H3F8UkGMmhBGQkG1hs0SyE39TSrwvgYZxNKnePKSYYSbAjnJIeUYuPXzp4HD8zSF1ylR3KvnDM2Q5e8dAoUFkAsL+yRoDtmDwmQsxOZ2sC1reFdERRc5VHTqSedBVNdkEFiEBJ7Qb30GTts0AJnS5CFpAdRXiHg3ec/3aFHt3gnXCKXcOwlXZQqr4GGVFGyi3O586WODJeDHeUKn/3BT3+EJb4Fz0eM/Nh/GkjgP4jH/wBnX+8FF0aiaF/1D5iOoWVrSo5lJ9Yk7BRZuI4Yfnk/BWqfifZhjEs0QOZlDum+a4uWjPPfVd+nK/v4N/8AXlb7Ecj/AAQ10LCwDuY1PJFt1BCjUV3idUunqUrRtfmHMUhYTLAUHBft0uOfzha7F64UnrLCP2ZIv5Vf/CBje6wL2Nmd0QfHOfkpogIEjVUUBS04LAbHwSMWA6EqPfRzB4FWzCTUMpXLodwdSp0vSoKCqgLAs79zwXMWZlQlYL9XMbsb8rxx7szx1hLEXUeFlFx4dMwJvyOldQy5WkXpJJb0Lll/6Sp99Zi/wccOkQAd5FJaxdLAMepTVRfytXmJdVkEZcNKEUSaWJKotnt+chTa/smia8SlSwZYY67ciPE8ojXTqiegGep8OTs7H57uXj29e1A06g2JF/UVlKWhVAuB+lSNAp1tb36f5ChWDntoaauAcJM92LZI1Ni25J3svnbnyuK7KnT6ZZ6JTPnsw3e3fA6pClrCAHOkS8JgsmWNWZ21kIVrnmFUHXINNeZ16WKYqIRRxRBcpdjJILWN9hcen3V79LhiQxQFkzHWRVU3IBJC6WvodQpA157DVxTSOCSxtoC4sbbg2IB586n+J6WYSolRIudOyNXwjhypSxMVZgbe/F4q/hJhmnwX0aJCxMkWULe+umo6ZiNafMM6RdzhrjN3RyjqseRCf+oUK4GJ+8HeJmUA2cj2emVvPp51e4vhlzxStmATMrOntxhsp7waG+VlAIIIyu1wRcU9CwtKWDMAPCKDSrppkzEoKBUVBtAq7Hs+UVuPPO0ncxx5lZRqRoPEbnPsNB6/GrsCGKEqGzsmhPK+5Hu2qjBiXkdYxikkX60uYgAQobLEGkDGzMGBOUD2WtajMhyIcqXyqbKOem3vqCUBC8Zzi6ZNM+T0IsL9rkM/nHKo8C78UxmKdCEKIqE7MSiXsedu7+dNxhV8RmYZmiihVbknJeIFsqnRSbi5GtU555WuGSwOyhSup6Dc0Tw3HBKrLPJHkI8BiRu8QggWsC2bS+ulrbGltbPQqYQ7Ysnyte5yEWzKOdKopcpTKUnNssgLa++wRPWVv9GvH3kEomUC5BFmtzII9r4VThxSvsdenOlpQQH0OoLjxFoUkscKrHnExNaF7VjGoXNRZ4iowO4nwWCa5t3b/aQDX9Jdj6ix86UeK8Glg1YZk5Ouq+/mp8jb309s1R5vnoehHQjmPKm9HxafTsknEnY+huR2XGwgCdSy13Fjy9/fnHNhc6AX9KPcF4aO6lkmQgZTlLDla+YDQ0Um4HGZFkjshDAsh/FsOdvsG19NR6VU7a4llyRbAjMw66+H3bmtZSV0mqS8s3GYOY7tuYtuxtCmbKVLLHxhXRxcEi4vqOvXWieM4WDKqQksHQOuYgZt9Adthz6GhBreOZlIYEgrqD056UXFZjbERMjFWFmG4uDb4UXw3ZuVxmuFFgfECCdNbLv8bXoHI5Ykk3JuSet966bwsZ4Y5CQqlFOY7ba2G7HyFQWtKElSywGpjoSolkhzAjspjDGYyVuPxcd42EmbxM+ZhcKm1ttQedOkhdyCzegAtvv50OwTK7HKDZeZ3J6kbD0q9MfCw16G3uv8jWbmLlTFqXL/ACm9/M8nPvQbGkSsSUiYzt5ae/3ijiYzzIZDobAXXlcHqOnlagyncHcEqfUEg/MUUxDsFQJot2zADTcWv00vQeKS7Sfpn91SfmTSurSlgREeIyhgTMGtv35284D9ulvgJ/Luz/7qj+NIXZ4/UY/+zD98V0TtYL4LED8y/wACD/Cuddnz9Tjv7N/jFMqA/wCwqH6h80wPI/o7D6xb7BG02Ib7ODxB/wCjSulpoAPIVzr8H6XbFH/l2T9sha6LeqONH+f72EUTfyI7/nHqoMykgXF7HpfemHAlQpy+JhqRz52pWfEDbc7aAmrmC41AhLPIFcgJlYgG5IA563NqDpJgSogwbw2YgpXLe+be9tYZo5M6hl0vqL8vUetI3GuGPh8qwAiNmszXZmTw7am5BA0P5pG4BLzAQyi2lwNvShePHeRryZ1X3FvZI/RcBv1aPnfkJYG2u+/dn5QwQiWZgUtLhJBbvyO495PHM3wstz4G9+Yn41lNgx8zgNq1wDctc7cyRy291ZStMucoBQUi+6wD3h7HcaRtk8SmJDBFuQLd0JkMh639Lae411GGDLhoYQQi92JJWLBbIbEqWJ0zM2W/QHoK5pDApZLNzAa4ta/Pc6eddOwWDMkbNM6mFkMKgQ5HdQLayM5JXMdLAXIO21MeK8HmUyhInEdYO6XyBuLgas9sm3aM1WUVHLWJ1MkhyxGgfbbWz2bIARX4VkEqyvPDoCECNdVGwANgPMnyA5UKxuMlWVpJI7xs5ySRkOpF/CCwJsbW/lR0YKFcMO8tGyuIg1vDdh4C1tlJNr8jSZi8ZHC7QKLy2IygWKvofFb87nsRrtQiaeXJTgQkkP7IiSZyZcwM5P5e58w2+esdG4NxSIqAZFB/ONvvom00ZHtpbqHH3g1zVhfbb7/Sp8LjO6dJAAxU3CnY+R00pgFMGiybwxJJUkl9rfZof/pEV/xqMeQ7zMfct/uFU8X2ggS4uzMOQXn+tahKY6co2JTFLmX28ORZAL6BY76gjW+52vypeknzsW5kknyubmvBZiqRQpWTiu1jfI7F0jtcWOhjfiHFpp5QI1VbHNdiNLa6k6D5mmPiLQ4lZO7eESCbOnd5Q5GUDx9XuTvuPWlOdgoJyFjb8m1/mQD/AOelEOzOCE3fS4j6vu1QFRrza15FJUtqBa5OlCTUY3QR1SLkZxKpSEzU2ISkWYPnvydhp3RcwGNfDSB2Uo9/rFscko5so5OBy3IBHisCNuKRBJ5FXYEMvowDD77e6q0rYqL2RmjkPhEqCRIlG9webaALpzPSoy7lmeR1YkKNFCKoUWACg6aUvTTJpwpGJ3IIG2h8fTaEHF1y8eAZiLcWLI31HzqcSg7Gh6OCLggjqK2qDQoEwiLjVoagWYjzqL/SsObL3kebpnWpIlLW+BJLZsHbmWyHOJFYi2agxeGSVcsi5hy5Mv6Lcvu6ipM4rwsK5LWUkKSWIyIjhYhjAGfsqp9ie3k6n95b3+ArROyf28Sg/RR2P/UFFMBIrUtTMcZrAGxDwH0+cUGnk7eZ+sVcJwXDRa5DK3WWxHujHh/azVclmLbm/L0HQDkPKoya9oGdUTZxxTVE9vpt3RYGSGSGEWOFYgKXU89R8KOo4JPMGx+Vv4UoYxNMwvpvbf1HpVUYuYey3pzB9DRVPU4EsRGmoEJq5QCVMoWI9fAQT43gnue7naIW1ACHS/tDMDqL2O4sAbe1VfCYdY1CLc7kkm5JJuSTzJJJvVCd3lZYmOtw8tj7KghkS/VmCm32VP2hRQmh5yny8IU8SZM7AFO3g/KBvaUXws4/4b/uE1zPgf4rGf2f/GK6tjmXu2LjMoBzDTUWN9DodOVK2F4lhXaVu48CRWc5bFlzrl8FwDbXQ8jb0OolzBJUlKCQVC40Yi3fpeLKXCUJJLEYrb29IFdg9sUfKBf2sQo/jT5icQqAsToPifQc6WYMesqTtBFlK9wC2XKXP0hMvh9CfiK6J2NjbuSX/GPI1yOgtlA6WHLqTQ3G6gy0mpWACVYcBLK/KkuzflbXe0dTRGaRmUJF1DW5cA3DsDu0JmBxp+sZiBd7hbgEAqAABzsV1Pn51bw2Mic5TArudFdifD5hRoTz1pj49wKJrs6DXaRPC3vto3oa51KJIJ2BYMFIykbEEXNxy00I5E0qpJiJ6ioBjmx9Pu0e/gBqeIhUlI6NRJAd2YEsSb3IYHnvHUMFj1WMAkEgaW9NKCy8RzSIi+yoRSeWmpPw++l3D8ehFy2dSBzR2HTdQb71Pw/Fpi5e7F0jbR5JBl7z/hIv5AfUFm5XAFzcaBUqpVIKxLVhAJJCT+UB3dm00h+Uy5RUwJVszMeezftDZwbhbNBG3cnxKG/K56/xrK3m7bYSFjGwkzJ4TlC2BGhA8XLb3VlZ7+BcYqP5yaWaQrrDvvtAg4siWMGMWtmdO+Ob4ZnYKFCkE5cup8R0t1U+VxtXQuEzzpGkcyGRVVVXLkWSOw5Kbd4PUg9L0lTYKbh8/iUEka2O4vYlG5NoRtXQuzWIimi7wLm5EHQqRuCvX+dP/iH4omz5UkISAAHUd1ZZaBstXN2CYbVsnCnGLpP5VD3bmDtlAnj57zDPFhs05cr3pAs6AHMq9yTnBJsSbcqAYJSiAMCBzuLW8j0HrTN20lTu0so7zPZG0DqADmKtoRyG4pawnE8flFlM45iWEyMNBfxCxtrvfW1FcNpamq4d+NwhKHOarnDmQ4bMEAPmDFVPUTJaCVAM+ZOG9hr1dhmGAyi4JBa4II8q1VD7XOthdjebh8I81nMR/ZdTb41exPDIQ+GGWVO8SWWRe9z5EjQsMrDS7EDqN6Fl1MqaeqoFtiDF38RkPhBvyKVZB/6VKiThpwrK6zF0k3RwpYDTYqAefxB3oRiHVbahT5kfOiX+jIHiwzrHKvf4mKIB5hJdCbyN4dBoCPIihM/FHjJMHD4I1uQsrrJKWAJAbXKutr86l0qCBfPLm+TbvyePU9Umar+Q63uA6W7nItyv3RYwMUknhjidzzspt67UcWdMNBHFLPDDKsjMY2ljBYEAhiM2hFra2/hSmuMxuMuGxMqourKgWKMKBr4Ut877+6pI8D3cbRobIWDOLLckcyx1NulWyVUq19FPm9GXa6TbtOQvv3tFPEvx6UvLlJU12xOcjbR7HIP+kkiGPG9psMImSOSOdyy5U+sKqc2pzLYEKCTo3ICg4kLauRcchcKPiSfiTWv0CLu9pMxvqNAFU733JuPIetCsZxiXIsLHwoSLoFXMw9pmN9TY21OlLK9UmdMKKdRKQMyGJ87J2BL8ozXEeD8TqmKUAkjrBBDjkSSmzZM4JFncElMHiMsgUEEStYAHxK2mtvst16g9ac04ZEY+dx+Vre/M22t5VzjsrjFOKN90jZxfrdVvbyDmul4LGpkA9aQcQmTEFKEEhmy17e6PcLoFIkEzU3fCxFwBuDkfRoQe2GJaMiHYnVmHMcrHmDr8KEnhqEIEbvCyjN4SMrH8kX9q3WmjtjhBOngIzxvZTr7JGxI6HX9alrucSi5pAh2F7sbaach0r6x8L8Wo/wABJkoWETSTiS4xKWTY83thvYWctCXiNGpFV0KS5JGEduXnaCvAJnVnge901s241ysuvQ/fRq9J007B2e/iJIzK17m+p30B1PnpRXC8VZVtIDfYMdL72DEnmQdfWg+P/D85cxVXTgF7qSM3brEaFzcgXckh8obfwaplSgSyjsPLNgdHbzF4OXrL0I4dxdZSwc5CAT4VJv0C66epbl7qswzHrf15Vh1LCSxgqZ8P1stClLABD2e5A20vpfwNovXrL1DLOq7kCvHcDUm1TAe0JImzVRnwIJusjx33yEAH3EGx8xatJuKRobG9+q2IHqb6e6rmEBlICWN9b8rdSelET6afSoTMnJKEqyJDP4jvY3a4tF/QzpZScJGLLMP7+8QYPCJEuVBzuSSSzHmWY6sfM1LNKFFz5fM2HzNXuI8NaFc5YMvOwtbp7qocRwEncu7Ky6FlGR76agk7LQIqZSxiCnf3q0XyuHVM0kJS3M5eTv3O2rC8UuJuTDJdbaHmOhFInDT9Xi/6kfetOGOxSthsRlbMAqm9iPaYA6H30n8MH1eN8oB961qeEpUOHrKszMT5FMC0M0zBLKs8Kz5qHpBDs6hbD4lRuViI90kRp07EcX8Do12aN8+97rbK1vQgH0pO7KaxYsf8q7fsqG/hTDw3h6wLdSCxGYvvvzDdKI+IqiQKOoppoJUtaShtCEpuSdGSxGZFgMyK01n4ToprOGII36yj5AuP3hj7R8dQROVYXynKL87afO1c3LfmE31JJ3PM3o72lx6SOBGmRAQDdVu5F7sSBpc8hptWX+jt4HUkpqwUMtnUEizCxI291T+FeAUYo+nnJxlRLB1BICbaAF33GzXvB9V8Sz5SgmiJQN2SVE3sHcNs2ZG0BcF4VNhmN9gNOfwtppsauLdXQk5QCTa7XF9D6MQPLltRrhPB0AMmRWyWuHO9zYALufO1FOGYKN5lvGthdz4Ry1HztRUz4zoKUdHJlLMtIYEMLAMGBOVtWJ8Y18lFUmViqiCsZt4sbM97tbaEuDCMyhsjNfnbfzrK7UBWUl//AEv/AIT/AKn/AM4xJ+H0Ev0h/wAv/tCD+EniAeWJFUBrEk8/Ectr/q3v50Gk4X9Df62VxIPEFhJz/wD5GsB869rKLVxCfw3hVHLpSEmelSlqYFRLBRN3Fyoi4LBgGaKanFPrFIWosgsm+QvltuWuTcxNiu0crlbKgygKCVV3ty8TC1ydTYCqL45z7cjN6kn5VlZWZXJQQkG+HJ7tyD5DkIgrr9ZVzzv84Y8NLh8LhosRJEJZJS2RW9hQCRcjmdPu2rXhXHnxeNjugYFWhKp4QqMCDYMdxmv7q8rKbSZaUyXA3jd8NpZMnhxmJSMRSpzrrFntRxM4KbDRotkw6gx59Q7c2IU3G46a35VJBxaLEQylY+4lhXP9VcIygqrDLewPiH86ysrk6TLmU5Cg9vWOVFDTTuE9dAdKARycl/f2irHiTrdU8XtEKFJ1vqVtfUDe+1bJEG/F6HTwvz9CB/KvayskuYpQxEl4wNPxSrp5TS1kAjJ7XAy1TnYpIMetOWsb3yKFUHlYgZTbfxG5PO5pQ4xhyHcK5aPPex3VmF2y6/PyFZWVr/hPh0iunTunc4cAFz/Wpie0NbTcGNLwHidR/ML3BSchcqOEk9yA2WvJhsEgbXlt5+VE+H8VmQHISAeRN7eY6VlZWg4BwujqaRU6olhZxYes5AACTYOz3zz2aLfi/jNXJqUSZSgkYAosA5JKhmXsGsA2Zd7MXw/HUyFGguwNw/eNcEhbkgCzbVBi8cGjsebLcDexDZrX06b1lZStFDIpfiGXJlBkhaWGeaUqzNyQVEglzkSSbxlZK1Ta6XOmF1FQL23YZNlp3RBhsC11FgAw2OUm4IIsbaefkOe1V+M8NEDhLgnfS9hzG48/lWVlKVcdrOIS0zpimdwySQnXR7nmSbWyj69RS0pmhLZgnz95Rb4di4YIyzLndxYG2qgNrlJ0BNrX3sTVnCcXSQ5QGDHbYjfb4VlZWqkfDNDO4YKpeIzChSnxGxDkAD8rdoJ5vHzvj3xBWSayfJlkAC2V7pF3Ovlyi2+XKedxqTzoTxDFlfqiQ2Q/nZWAW2uxzDboSL6isrKy/wAKEnikp9z/ANp+sJeAEiuQNyfJJikNRfNYHQi7dACdjya3x5Uc7H44CYgsyxWJcHUWFrWUDe5PxrKyvqHGKZE6hmy13Cgx77P2jMHQ3jfVKimSsjT6w8YnikTKHBzoXVToRqXW2hH2rUQMQkG+U66/zrKyvgnGqCXQV66eWSUpU18zYZsB5AQslTCqUF6s/moeghC7bQqkE5CqHYgNYWzWa4vy660hcKH1eP8A7OP3qysrecE/3Mk/rHzTC2p/t1v7noYIdjEzGZPt4WZfjE4ooHkK+AFly6XbTNe2ZRcW061lZRPxBXqpZM1KUpUFlCTiBLOJlwxHWDWN22gzgdFLrZE2RNyvkzhnZnBgZI4ZcxGo23GtyL3B86mZXYorixfxZgb6XsBa9rX8r1lZRVPXzaSZTSJQAQZaVKDDrFSSSSc78iIc8MpZMvhxUlIdi51LJxX3c27LBhDJw/hZmKxu2UG+ZvsWF3IA1Ps6VJ2c4vdnRlF1UDMLjOudTdhrZtr20rKyvnZAXSuRr8in6/WMXwevqJtQJq1kmYes+R/KRbIM9mZhbKHL/SSdDXlZWUj6FMbDoER//9k=.jpeg" height="100" /></a>


<p><a href="https://king.com/game/candycrush">Candy Crush Saga</a> is a hit mobile game developed by King (part of Activision|Blizzard) that is played by millions of people all around the world. The game is structured as a series of levels where players need to match similar candy together to (hopefully) clear the level and keep progressing on the level map. If you are one of the few that haven't played Candy Crush, here's a short demo:</p>

In this Project, we will get to work with a real Candy Crush dataset and use this data to estimate level difficulty.
