.PHONEY: help
help:
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
	 sort | \
	 awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONEY: watch
watch: ## Watch for changes and serve preview of site with drafts
	bundle exec rake clean
	bundle exec rake preview

.PHONEY: develop
develop: ## Serve a preview of the site without drafts and refresh changes
	bundle exec rake clean
	bundle exec rake develop

.PHONEY: new_adventure
new_adventure: ## Start a new adventure post
	bundle exec rake new_adventure

.PHONEY: new_post
new_post: ## Start a new post
	bundle exec rake new_post 

.PHONEY: deploy
deploy: ## deploy
	./deploy.sh

.PHONEY: publish_draft
publish_draft: ## Publishes a draft
	bundle exec rake publish_draft

.PHONEY: unpublished
unpublished: ## List drafts
	@ag -l 'published: false' source/_posts/
