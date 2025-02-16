.PHONY: help
help:
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
	 sort | \
	 awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: watch
watch: ## Watch for changes and serve preview of site with drafts
	./watch-preview.sh

.PHONY: develop
develop: ## Serve a preview of the site without drafts and refresh changes
	./watch.sh

.PHONY: new_adventure
new_adventure: ## Start a new adventure post
	bb new-adventure

.PHONY: new_post
new_post: ## Start a new post
	bb new-post

.PHONY: deploy
deploy: ## deploy
	./deploy.sh

.PHONY: publish_draft
publish_draft: ## Publishes a draft
	bb publish

.PHONY: unpublished
unpublished: ## List drafts
	bb list-drafts

.PHONY: render
render: ## Generate site using bb render
	bb render

.PHONY: render-preview
render-preview: ## Generate site with drafts using bb render
	bb render --preview true
