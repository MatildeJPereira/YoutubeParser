from urllib.parse import urlparse, parse_qs

from selenium import webdriver
from selenium.common import TimeoutException
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
import time
import networkx as nx
from selenium.webdriver.ie.service import Service
from selenium.webdriver.support.wait import WebDriverWait
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.support import expected_conditions as EC

# TODO there is a "get youtube premium tab" in front of useful things sometimes. make a function to get rid of that.

INITIAL_URL = "https://www.youtube.com/watch?v=CpCKkWMbmXU"
DEPTH = 3
NUMBER_RECOMMENDATIONS = 5
KEY_WORDS = ["gaza war"] # "gaza occupation", "israel war"]
NUMBER_SEARCH = 2


# start a new webdriver altogether, useful for when the session breaks
def start_new_driver():
    options = Options()
    options.add_argument("--headless=new")
    options.add_argument("--window-size=1920,1080")
    driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)
    driver.set_page_load_timeout(30)
    return driver


# avoid repeating webdriver waits
def wait_for(driver, condition, timeout=60):
    return WebDriverWait(driver, timeout).until(condition)


def accept_cookies(driver):
    driver.find_element(By.XPATH, "//*[@id=\"content\"]/div[2]/div[6]/div[1]/ytd-button-renderer[2]/yt-button-shape/button").click()
    print("Accepted cookies")



def get_video_title(driver):
    video_title = wait_for(driver, EC.presence_of_element_located((By.ID, "title")))
    return video_title.text


def get_channel(driver):
    channel = wait_for(driver, EC.presence_of_element_located((By.XPATH, "//*[@id=\"text\"]/a")))
    return channel.text


def get_views(driver):
    # if the description isn't open yet, though it should
    if driver.find_element(By.ID, "expand").is_displayed():
        driver.find_element(By.ID, "expand").click()

    # if the view-count element has the properties of a normal video, get the views from info element
    # else this is a stream, so get the views from the view-count directly
    info_container = wait_for(driver, EC.presence_of_element_located((By.ID, "info-container")))
    view_count = info_container.find_element(By.ID, "view-count")

    element = wait_for(view_count, EC.presence_of_element_located((By.XPATH, "//*[@id=\"view-count\"]/yt-animated-rolling-number")))
    if element.get_attribute("style") == "":
        views = wait_for(driver, EC.presence_of_element_located((By.XPATH, "//*[@id=\"info\"]/span[1]")))
        views_num, _ = views.text.split(" ")
    else:
        print("This is a stream.")
        views = view_count.text
        views_num, _, _ = views.split(" ")

    return views_num


# TODO change to the actual number?
# This is returning the text value of the button, not the actual number of likes
def get_likes(driver):
    likes_xpath = "//*[@id=\"top-level-buttons-computed\"]/segmented-like-dislike-button-view-model/yt-smartimation/div/div/like-button-view-model/toggle-button-view-model/button-view-model/button/div[2]"
    if driver.find_elements(By.XPATH, likes_xpath):
        return driver.find_element(By.XPATH, likes_xpath).text
    else:
        return "Emtpy"


def get_description(driver):
    if driver.find_element(By.ID, "expand").is_displayed():
        wait_for(driver, EC.invisibility_of_element((By.ID, "tooltip")))
        wait_for(driver, EC.element_to_be_clickable((By.ID, "expand"))).click()

        # driver.find_element(By.ID, "expand").click()
        if driver.find_elements(By.XPATH, "//*[@id=\"description-inline-expander\"]/yt-attributed-string/span"):
            description_span = driver.find_element(By.XPATH, "//*[@id=\"description-inline-expander\"]/yt-attributed-string/span")
            return description_span.text
        else:
            return "No description available"
    else:
        return "No description available"


# the transcript is currently the raw text, with no timestamps.
def get_transcript(driver):
    # if the description isn't open yet, though it should
    if driver.find_element(By.ID, "expand").is_displayed():
        driver.find_element(By.ID, "expand").click()

    # click on show transcript if exists
    try:
        wait_for(driver, EC.element_to_be_clickable((By.XPATH, "//*[@id=\"primary-button\"]/ytd-button-renderer/yt-button-shape/button")), 10).click()
        try:
            raw_transcript = wait_for(driver, EC.presence_of_element_located((By.ID, "segments-container")))
            transcript_lines = wait_for(raw_transcript, EC.presence_of_all_elements_located((By.TAG_NAME, "yt-formatted-string")))
            transcript = " ".join([transcript_line.text for transcript_line in transcript_lines])
        except:
            print("Transcript took too long to load")
            transcript = "Empty"
    except:
        print("No transcript available")
        transcript = "Empty"

    return transcript


def initial_search(driver):
    initial_urls = []
    for word in KEY_WORDS:
        word = word.lower()
        word = word.replace(" ", "+")
        driver.get(f"https://www.youtube.com/results?search_query={word}")

        # if the page is asking for cookie consent, accept it first
        if driver.find_elements(By.TAG_NAME, "ytd-consent-bump-v2-lightbox"):
            print("There is a consent box, accept it")
            accept_cookies(driver)
            time.sleep(10)

        videos_elements = wait_for(driver, EC.presence_of_all_elements_located((By.TAG_NAME, "ytd-video-renderer")))
        count = 0
        for video in videos_elements:
            if count == NUMBER_SEARCH:
                break

            # get the SHORTS tag and skip it
            if "SHORTS" in video.text:
                continue

            url = video.find_element(By.ID, "thumbnail").get_attribute("href")
            initial_urls.append(url)
            print(url)
            count += 1
    return initial_urls


# This gets the v parameter value from the url, the video id
def get_video_id(url):
    parsed = urlparse(url)

    if parsed.netloc in ["www.youtube.com", "youtube.com"]:
        qs = parse_qs(parsed.query)
        return qs.get("v", [None])[0]

    return None


def get_recommended(driver):
    recs_links = []
    try:
        related = wait_for(driver, EC.presence_of_element_located((By.ID, 'related')))
        items = wait_for(related, EC.presence_of_element_located((By.ID, 'items')))
        recs = wait_for(items, EC.presence_of_all_elements_located((By.TAG_NAME, 'ytd-compact-video-renderer')))

        # recs = (driver.find_element(By.ID, 'related')
        #         .find_element(By.ID, 'items')
        #         .find_elements(By.TAG_NAME, "ytd-compact-video-renderer"))
    except:
        recs = []

    for rec in recs:
        rec_link = rec.find_element(By.ID, 'thumbnail').get_attribute('href')
        recs_links.append(rec_link)
        if len(recs_links) >= NUMBER_RECOMMENDATIONS:
            break

    return list(set(recs_links))


def get_video_data(driver, video_url):
    # if the page is asking for cookie consent, accept it first
    if driver.find_elements(By.TAG_NAME, "ytd-consent-bump-v2-lightbox"):
        print("There is a consent box, accept it")
        accept_cookies(driver)
        # time.sleep(10)

    video_title = get_video_title(driver)
    video_likes = get_likes(driver)
    video_channel = get_channel(driver)
    video_desc = get_description(driver)
    video_views = get_views(driver)
    video_transcript = get_transcript(driver)
    video_recs = get_recommended(driver)

    return video_title, video_recs, video_desc, video_views, video_likes, video_channel, video_transcript


def main():
    driver = start_new_driver()

    graph = nx.DiGraph() # THIS HAS **URL IDS** AS IDS
    visited = set() # THIS IS A SET OF **URLS**
    to_visit = initial_search(driver) # THIS IS A LIST OF **URLS**
    #to_visit = [INITIAL_URL, INITIAL_URL] # testing
    d = 0

    for _ in range(DEPTH):
        to_visit_tmp = []
        print("Depth: ", d)

        for url in to_visit:
            try:
                video_id = get_video_id(url)
                print("Current video ID: " , video_id)

                # if the video is already visited only get the new recommended videos from it, no need to reload other info
                if video_id in [get_video_id(v) for v in visited]:
                    driver.get(url)
                    time.sleep(1)
                    recommendations = get_recommended(driver)
                    print("Node already exists:", graph.nodes[video_id]["title"])
                else:
                    driver.get(url)
                    time.sleep(1)
                    title, recommendations, description, views, likes, channel, transcript = get_video_data(driver, url)
                    print("Title: ", title)
                    graph.add_node(video_id, url=url, title=title, depth=d, description=description, views=views, likes=likes, channel=channel, transcript=transcript)

                    # print("Recommendations: ", recommendations)
                    # print("Description: ", desc)
                    # print("Likes: ", likes)
                    # print("Channel: ", channel)
                    # print("Transcript: ", transcript)

                for recommendation in recommendations:
                    rec_id = get_video_id(recommendation)
                    graph.add_edge(video_id, rec_id)
                    if recommendation not in visited:
                        to_visit_tmp.append(recommendation)

                visited.add(url)

            # if the page takes way too long then reset the webdriver
            except TimeoutException:
                print(f"Timeout error, could not load {url}")
                driver.quit()
                driver = start_new_driver()
                continue

        d += 1
        to_visit = to_visit_tmp

    # This is to avoid empty nodes at the end
    print("Videos that still need to be visited:")
    print(to_visit)
    for recommendation in to_visit:
        driver.get(recommendation)
        time.sleep(1)
        video_id = get_video_id(recommendation)
        title = get_video_title(driver)
        desc = get_description(driver)
        views = get_views(driver)
        likes = get_likes(driver)
        channel = get_channel(driver)
        transcript = get_transcript(driver)

        print(title)
        print(recommendation)

        if video_id in [get_video_id(v) for v in visited]:
            print("Node already exists:", graph.nodes[video_id]["title"])
        else:
            graph.add_node(video_id, url=recommendation, title=title, depth=d, description=desc, views=views, likes=likes, channel=channel, transcript=transcript)

    driver.quit()

    nx.write_gexf(graph, "graph_hamas_3.gexf")


if __name__ == "__main__":
    main()

