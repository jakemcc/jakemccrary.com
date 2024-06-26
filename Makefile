ifeq ($(WAS_SOURCED),)
  $(error WAS_SOURCED is not set, source Envfile)
endif

.PHONY: help
help:
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
	 sort | \
	 awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: watch
watch: ## Watch for changes and serve preview of site with drafts
	bundle exec rake clean
	bundle exec rake preview

.PHONY: develop
develop: ## Serve a preview of the site without drafts and refresh changes
	bundle exec rake clean
	bundle exec rake develop

.PHONY: new_adventure
new_adventure: ## Start a new adventure post
	bundle exec rake new_adventure

.PHONY: new_post
new_post: ## Start a new post
	bundle exec rake new_post 

.PHONY: deploy
deploy: ## deploy
	./deploy.sh

.PHONY: publish_draft
publish_draft: ## Publishes a draft
	bundle exec rake publish_draft

.PHONY: unpublished
unpublished: ## List drafts
	@rg  -g '*markdown' -l 'published: false' source/_posts

.PHONY: build
build: ## Generate site
	bundle exec rake generate
